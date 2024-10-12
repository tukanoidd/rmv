use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    path::{Path, PathBuf},
};

use bon::bon;
use derive_more::derive::From;
use iced::{
    futures::TryStreamExt,
    widget::{
        button, center, column, image, markdown, row, scrollable, svg, text, toggler, Rule, Space,
    },
    Element, Length, Renderer, Task, Theme,
};
use iced_fonts::{nerd::icon_to_string, Nerd};
use infer::MatcherType;
use miette::IntoDiagnostic;
use rfd::AsyncFileDialog;
use tokio::fs::read_to_string;
use tokio_stream::{wrappers::ReadDirStream, StreamExt};
use uuid::Uuid;

pub struct App {
    file_tree: Option<FileTree>,
    selected: Option<Vec<Uuid>>,
    dirs_expanded: Option<HashSet<Uuid>>,
}

impl App {
    pub fn new(target: Option<(Dir, Option<PathBuf>)>) -> (Self, Task<AppMsg>) {
        let res = Self {
            file_tree: None,
            selected: None,
            dirs_expanded: None,
        };
        let task = target
            .map(|target| {
                Task::perform(Self::init(target), |res| {
                    AppMsg::InitResult(res.map_err(|e| e.to_string()))
                })
            })
            .unwrap_or_else(Task::none);

        (res, task)
    }

    async fn init((target_dir, target_file): (Dir, Option<PathBuf>)) -> miette::Result<AppInit> {
        tracing::info!("Initializing the app...");

        let file_tree = FileTree::new(target_dir).await?;
        let selected = target_file.and_then(|f| file_tree.find().path(&f).call());

        Ok(AppInit {
            file_tree,
            selected,
        })
    }

    pub fn title(&self) -> String {
        "Rusty Media Viewer".to_string()
    }

    pub fn view(&self) -> Element<'_, AppMsg, Theme, Renderer> {
        match &self.file_tree {
            Some(file_tree) => AppPage::Viewer {
                theme: self.theme(),
                file_tree,
                selected: self.selected.as_ref(),
                dirs_expanded: self.dirs_expanded.as_ref(),
            },
            None => AppPage::Welcome,
        }
        .view()
    }

    pub fn update(&mut self, msg: AppMsg) -> Task<AppMsg> {
        match msg {
            AppMsg::PickTarget(ty) => {
                return Task::perform(Self::pick_target(ty), |res| {
                    AppMsg::Init(res.map_err(|e| e.to_string()))
                })
            }
            AppMsg::Init(target) => match target {
                Ok(target) => match target {
                    Some(target) => {
                        return Task::perform(Self::init(target), |res| {
                            AppMsg::InitResult(res.map_err(|e| e.to_string()))
                        })
                    }

                    None => tracing::warn!("No target was chosen..."),
                },
                Err(err) => tracing::error!("{err}"),
            },
            AppMsg::InitResult(res) => match res {
                Ok(AppInit {
                    file_tree,
                    selected,
                }) => {
                    self.file_tree = Some(file_tree);
                    self.selected = selected;
                }
                Err(err) => panic!("Failed to initialize app: {err}"),
            },

            AppMsg::ToggleExpandDir(id) => {
                if let Some(dirs_expanded) = &mut self.dirs_expanded {
                    match dirs_expanded.contains(&id) {
                        true => dirs_expanded.remove(&id),
                        false => dirs_expanded.insert(id),
                    };
                } else {
                    self.dirs_expanded = Some(HashSet::from_iter([id]));
                }
            }
            AppMsg::SelectFilePath(path) => {
                if let Some(file_tree) = &self.file_tree {
                    self.selected = file_tree.find().path(&path).call();
                }
            }

            AppMsg::MarkdownUrl(url) => {
                tracing::warn!("TODO: handle markdown url msg");
                tracing::warn!("URL: {url}");
            }
        }

        Task::none()
    }

    pub fn theme(&self) -> Theme {
        Theme::CatppuccinMocha
    }

    async fn pick_target(ty: PickTargetType) -> miette::Result<Option<(Dir, Option<PathBuf>)>> {
        let dialog = AsyncFileDialog::new();

        let path = match ty {
            PickTargetType::File => dialog.pick_file().await,
            PickTargetType::Folder => dialog.pick_folder().await,
        };

        Dir::new_open(path.map(|p| p.path().to_path_buf()))
    }
}

#[derive(Debug, Clone)]
pub enum AppMsg {
    PickTarget(PickTargetType),
    Init(Result<Option<(Dir, Option<PathBuf>)>, String>),
    InitResult(Result<AppInit, String>),

    ToggleExpandDir(Uuid),
    SelectFilePath(PathBuf),

    MarkdownUrl(markdown::Url),
}

#[derive(Debug, Clone)]
pub enum PickTargetType {
    File,
    Folder,
}

#[derive(Debug, Clone)]
pub struct AppInit {
    file_tree: FileTree,
    selected: Option<Vec<Uuid>>,
}

enum AppPage<'a> {
    Welcome,
    Viewer {
        theme: Theme,
        file_tree: &'a FileTree,
        selected: Option<&'a Vec<Uuid>>,
        dirs_expanded: Option<&'a HashSet<Uuid>>,
    },
}

impl<'a> AppPage<'a> {
    fn view(self) -> Element<'a, AppMsg, Theme, Renderer> {
        match self {
            AppPage::Welcome => Self::welcome(),
            AppPage::Viewer {
                theme,
                file_tree,
                selected,
                dirs_expanded,
            } => Self::viewer(theme, file_tree, selected, dirs_expanded),
        }
    }

    fn welcome() -> Element<'a, AppMsg, Theme, Renderer> {
        center(
            column![
                text("Welcomer to the Rusty Media Viewer!").size(25),
                text("Please choose a file or directory:").size(15),
                row![
                    button("Folder").on_press(AppMsg::PickTarget(PickTargetType::Folder)),
                    button("File").on_press(AppMsg::PickTarget(PickTargetType::File))
                ]
                .spacing(5),
            ]
            .spacing(5),
        )
        .into()
    }

    fn viewer(
        theme: Theme,
        file_tree: &'a FileTree,
        selected: Option<&'a Vec<Uuid>>,
        dirs_expanded: Option<&'a HashSet<Uuid>>,
    ) -> Element<'a, AppMsg, Theme, Renderer> {
        let sidebar = {
            let file_tree = scrollable(file_tree.view(None, dirs_expanded, true, selected))
                .height(Length::Fill);

            column![
                text("File Tree"),
                Space::with_height(5),
                Rule::horizontal(2),
                Space::with_height(10),
                file_tree
            ]
            .padding(10)
            .width(Length::FillPortion(2))
        };
        let view: Element<'a, AppMsg, Theme, Renderer> = match selected {
            Some(id_chain) => match file_tree.get(id_chain) {
                Some(leaf) => match leaf {
                    FileTreeLeaf::Dir(_) => text("Please select a file").into(),
                    FileTreeLeaf::File(file) => match &file.ty {
                        SupportedFileTy::Image => Self::image_viewer(&file.path),
                        SupportedFileTy::Svg => Self::svg_viewer(&file.path),
                        SupportedFileTy::Markdown(items) => Self::markdown_viewer(theme, items),
                    },
                },
                None => text(format!(
                    "Failed to get a file through id chain: {id_chain:?}"
                ))
                .into(),
            },
            None => text("Please select a file").into(),
        };

        row![
            sidebar,
            Rule::vertical(2),
            center(view).padding(5).width(Length::FillPortion(8))
        ]
        .into()
    }

    fn image_viewer(path: &'a PathBuf) -> Element<'a, AppMsg, Theme, Renderer> {
        image(path).into()
    }

    fn svg_viewer(path: &'a PathBuf) -> Element<'a, AppMsg, Theme, Renderer> {
        svg(path).into()
    }

    fn markdown_viewer(
        theme: Theme,
        items: &'a [markdown::Item],
    ) -> Element<'a, AppMsg, Theme, Renderer> {
        markdown(
            items,
            markdown::Settings::with_text_size(12),
            markdown::Style::from_palette(theme.palette()),
        )
        .map(AppMsg::MarkdownUrl)
    }
}

pub enum OpenTarget {
    None,
    Dir(PathBuf),
    File(PathBuf),
}

impl From<Option<PathBuf>> for OpenTarget {
    fn from(value: Option<PathBuf>) -> Self {
        match value {
            Some(target) => match target.is_dir() {
                true => Self::Dir(target),
                false => Self::File(target),
            },
            None => Self::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, From, derive_more::Deref)]
pub struct Dir(PathBuf);

impl AsRef<Path> for Dir {
    fn as_ref(&self) -> &Path {
        self.deref().as_ref()
    }
}

impl Dir {
    pub fn new_open(
        target: impl Into<OpenTarget>,
    ) -> miette::Result<Option<(Self, Option<PathBuf>)>> {
        let open_target = target.into();

        Ok(match open_target {
            OpenTarget::None => None,
            OpenTarget::Dir(path) => Some((Self(path), None)),
            OpenTarget::File(path) => {
                let parent = path
                    .parent()
                    .ok_or_else(|| miette::miette!("Failed to get a parent directory of {path:?}"))?
                    .to_path_buf();
                Some((Self(parent), Some(path)))
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
struct FileTree {
    root: Dir,
    leaves: HashMap<Uuid, FileTreeLeaf>,
}

#[bon]
impl FileTree {
    async fn new(target: impl Into<Dir>) -> miette::Result<Self> {
        let root = target.into();

        tracing::info!("Creating a file tree for {:?}", &*root);

        let entries = tokio::fs::read_dir(root.clone()).await.into_diagnostic()?;
        let leaves = ReadDirStream::new(entries)
            .map_err(|err| miette::miette!("{err}"))
            .try_filter_map(|entry| FileTreeLeaf::new(entry.path()))
            .collect::<miette::Result<Vec<_>>>()
            .await?
            .into_iter()
            .map(|l| {
                let id = Uuid::new_v4();

                tracing::debug!("Assigning {id} to {:?}", l.path());

                (id, l)
            })
            .collect::<HashMap<_, _>>();

        Ok(Self { root, leaves })
    }

    fn view<'a>(
        &'a self,
        id: Option<&'a Uuid>,
        dirs_expanded: Option<&'a HashSet<Uuid>>,
        expanded: bool,
        selected: Option<&'a Vec<Uuid>>,
    ) -> Element<'a, AppMsg, Theme, Renderer> {
        let icon = text(icon_to_string(Nerd::Folder)).font(iced_fonts::NERD_FONT);
        let dir_name = self
            .root
            .file_name()
            .map(|n| n.to_string_lossy())
            .unwrap_or_else(|| "...".into());
        let arrow_icon = text(icon_to_string(match expanded {
            true => Nerd::ArrowDown,
            false => Nerd::ArrowUp,
        }));
        let mut header =
            button(row![icon, text(dir_name), Space::with_width(10), arrow_icon].spacing(5));

        if let Some(id) = id {
            header = header.on_press(AppMsg::ToggleExpandDir(*id));
        }

        let children = expanded.then(|| {
            column(self.leaves.iter().map(|(id, leaf)| match leaf {
                FileTreeLeaf::Dir(tree) => tree.view(
                    Some(id),
                    dirs_expanded,
                    dirs_expanded.map(|de| de.contains(id)).unwrap_or_default(),
                    selected,
                ),
                FileTreeLeaf::File(file) => {
                    file.ft_view(selected.map(|s| s.last() == Some(id)).unwrap_or_default())
                }
            }))
            .width(Length::Fill)
        });

        let mut element = column![header].width(Length::Fill);

        if let Some(children) = children {
            element = element.push(row![Space::with_width(10), children].width(Length::Fill));
        };

        element.width(Length::Fill).into()
    }

    #[tracing::instrument(skip(self))]
    fn get(&self, id_chain: &[Uuid]) -> Option<&FileTreeLeaf> {
        tracing::debug!("GET");

        match id_chain.is_empty() {
            true => {
                tracing::warn!("Empty id chain");
                None
            }
            false => {
                let id = &id_chain[0];

                match self.leaves.get(id) {
                    Some(leaf) => match leaf {
                        FileTreeLeaf::Dir(file_tree) => match id_chain.len() == 1 {
                            true => {
                                tracing::debug!("GOT DIR {:?}", leaf.path());
                                Some(leaf)
                            }
                            false => file_tree.get(&id_chain[1..]),
                        },
                        file_leaf @ FileTreeLeaf::File(_) => match id_chain.len() == 1 {
                            true => {
                                tracing::debug!("GOT FILE {:?}", leaf.path());
                                Some(file_leaf)
                            }
                            false => {
                                tracing::error!("File has no children");
                                None
                            }
                        },
                    },
                    None => {
                        tracing::error!("Failed to get a leaf by id {id}");
                        None
                    }
                }
            }
        }
    }

    #[builder]
    #[tracing::instrument(skip(self))]
    fn find(&self, path: &PathBuf, previous: Option<Vec<Uuid>>) -> Option<Vec<Uuid>> {
        tracing::debug!("FIND");

        let previous = previous.unwrap_or_default();

        for (id, leaf) in &self.leaves {
            let mut previous = previous.clone();
            let res = match path == leaf.path() {
                true => {
                    previous.push(*id);
                    tracing::debug!("FOUND {previous:?}");
                    Some(previous)
                }
                false => match leaf {
                    FileTreeLeaf::Dir(dir) => {
                        previous.push(*id);
                        dir.find().path(path).previous(previous).call()
                    }
                    FileTreeLeaf::File(_) => None,
                },
            };

            if let Some(res) = res {
                return Some(res);
            }
        }

        None
    }
}

#[derive(Clone, Debug, PartialEq)]
enum FileTreeLeaf {
    Dir(Box<FileTree>),
    File(SupportedFile),
}

impl FileTreeLeaf {
    async fn new(path: PathBuf) -> miette::Result<Option<Self>> {
        tracing::debug!("New leaf: {path:?}");

        match path.is_dir() {
            true => Box::pin(Self::dir(path)).await.map(Some),
            false => match Self::file(path).await {
                Ok(file) => Ok(Some(file)),
                Err(err) => {
                    tracing::warn!("{err}");
                    Ok(None)
                }
            },
        }
    }

    async fn dir(target: impl Into<Dir>) -> miette::Result<Self> {
        Ok(Self::Dir(FileTree::new(target).await.map(Box::new)?))
    }

    async fn file(target: impl Into<PathBuf>) -> miette::Result<Self> {
        Ok(Self::File(SupportedFile::new(target).await?))
    }

    fn path(&self) -> &PathBuf {
        match self {
            FileTreeLeaf::Dir(dir) => &dir.root,
            FileTreeLeaf::File(file) => &file.path,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct SupportedFile {
    path: PathBuf,
    ty: SupportedFileTy,
}

impl SupportedFile {
    async fn new(path: impl Into<PathBuf>) -> miette::Result<Self> {
        let path = path.into();
        let ty = SupportedFileTy::try_from(&path).await?;

        Ok(Self { path, ty })
    }

    fn ft_view(&self, selected: bool) -> Element<'_, AppMsg, Theme, Renderer> {
        let file_name = self
            .path
            .file_name()
            .map(|f| f.to_string_lossy().to_string())
            .unwrap_or_else(|| "...".into());
        row![
            toggler(selected),
            button(text(file_name)).on_press(AppMsg::SelectFilePath(self.path.clone()))
        ]
        .into()
    }
}

#[derive(Clone, Debug)]
enum SupportedFileTy {
    Image,
    Svg,
    Markdown(Vec<markdown::Item>),
}

impl PartialEq for SupportedFileTy {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Image, Self::Image) | (Self::Svg, Self::Svg) => true,
            (Self::Markdown(md1), Self::Markdown(md2)) => are_md_items_equal(md1, md2),
            _ => false,
        }
    }
}

fn are_md_items_equal(md1: &[markdown::Item], md2: &[markdown::Item]) -> bool {
    if md1.len() != md2.len() {
        return false;
    }

    md1.iter().zip(md2.iter()).all(|(i1, i2)| {
        use markdown::Item::*;

        match (i1, i2) {
            (Heading(l1, t1), Heading(l2, t2)) => {
                l1 == l2 && format!("{t1:?}") == format!("{t2:?}")
            }
            (Paragraph(p1), Paragraph(p2)) => format!("{p1:?}") == format!("{p2:?}"),
            (CodeBlock(c1), CodeBlock(c2)) => format!("{c1:?}") == format!("{c2:?}"),
            (
                List {
                    start: s1,
                    items: i1,
                },
                List {
                    start: s2,
                    items: i2,
                },
            ) => {
                s1 == s2
                    && i1
                        .iter()
                        .zip(i2.iter())
                        .all(|(md1, md2)| are_md_items_equal(md1, md2))
            }
            _ => false,
        }
    })
}

impl SupportedFileTy {
    async fn try_from(value: &PathBuf) -> miette::Result<Self> {
        tracing::debug!("Checking if {value:?} is a supported type...");

        let kind = infer::get_from_path(value)
            .into_diagnostic()?
            .ok_or_else(|| miette::miette!("Failed to infer a MIME type for {value:?}"))?;

        match kind.matcher_type() {
            MatcherType::Image => match kind.mime_type() {
                "image/svg+xml" => Ok(Self::Svg),
                _ => Ok(Self::Image),
            },
            MatcherType::Text => match kind.mime_type() {
                "text/markdown" => Ok(Self::Markdown({
                    let str = read_to_string(value).await.into_diagnostic()?;
                    let items = markdown::parse(&str).collect();

                    items
                })),
                ty => miette::bail!("Unsupported text MIME type {ty} for {value:?}"),
            },
            ty => miette::bail!("Unsupported MIME type {ty:?} for {value:?}"),
        }
    }
}
