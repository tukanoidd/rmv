# What
Media file viewer written in Rust + Iced

# Why
1. Wayland, Niri, NixOS, complications with existing alternatives
2. Fun

# Usable?
NOPE. I just started working on it, the ui is not styled at all, many sensible features are not implemented etc.

# But does it work at least?
I mean, I was able to view some png files i have lying around on my computer, but I havent tested anything else.

# What media types are currently supported?
1. Images - powered by `iced::widget::image`
2. SVGs - powered by `iced::widget::svg`
3. Markdown - powered by `iced::widget::markdown`

# Plans?
1. PDF
2. DOC?
3. Whatever I come up with later

# Only viewing or editing as well?
No certain plans for editing the content viewed, but depending on my use-case, might implement some minor 
editing features here and there
