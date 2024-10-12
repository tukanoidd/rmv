{...}: {
  perSystem = {
    pkgs,
    config,
    ...
  }: let
    crateName = "rmv";
  in {
    nci = {
      projects."rmv".path = ./.;
      crates.${crateName} = {
        runtimeLibs = with pkgs;
        with xorg; [
          wayland
          libX11
          vulkan-loader
          libxkbcommon
        ];
      };
    };
  };
}
