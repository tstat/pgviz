{
  description = "visualize foreign keys in postgres";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ my-overlay ];
        };
        my-overlay = final: prev: { };
        package-name = "pgviz";
        package = self.packages."${system}"."${package-name}";

      in
      {
        inherit pkgs;

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ cargo rust-analyzer rustfmt cargo-watch rustPackages.clippy graphviz ];
          inputsFrom = [ package ];
        };
        packages = {
          default = self.packages."${system}"."${package-name}-wrapped";
          "${package-name}-wrapped" = pkgs.runCommand package-name
            {
              buildInputs = [ pkgs.makeWrapper ];
            }
            ''
              makeWrapper ${package}/bin/pgviz $out/bin/pgviz --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.graphviz]}
            '';
          # Read the docs at https://nixos.org/manual/nixpkgs/stable/#rust
          "${package-name}" = pkgs.rustPlatform.buildRustPackage {
            pname = package-name;
            version = "0.1.0";
            cargoHash = "sha256-5pNtTKhPwT7BcqdUx0viU40dKIZgFNCkf6tBi6vl0D4=";
            src = ./src;
            buildInputs =
              let
                darwin-frameworks = with pkgs.darwin.apple_sdk.frameworks;
                  [
                    CoreFoundation
                    CoreServices
                    SystemConfiguration
                    pkgs.libiconv
                  ];
                system-dependent = if pkgs.stdenv.isDarwin then darwin-frameworks else [ ];
              in
              system-dependent;
          };
        };
      });
}
