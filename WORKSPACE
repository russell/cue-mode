workspace(name = "cue-mode")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Elisp

http_archive(
    name = "phst_rules_elisp",
    sha256 = "b5535b1d2a84a2fdeb85f6e73ff975585093a1e7f95b7b6d4f884bcef781dcaf",
    strip_prefix = "rules_elisp-a32205c1f7fdc95df47f31c31a29eca8208e948c",
    urls = [
        "https://github.com/phst/rules_elisp/archive/a32205c1f7fdc95df47f31c31a29eca8208e948c.zip",  # 2022-04-18
    ],
)

load(
    "@phst_rules_elisp//elisp:repositories.bzl",
    "rules_elisp_dependencies",
    "rules_elisp_toolchains",
)

rules_elisp_dependencies()

rules_elisp_toolchains()

# Nix
http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-a388ab60dea07c3fc182453e89ff1a67c9d3eba6",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/a388ab60dea07c3fc182453e89ff1a67c9d3eba6.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl", "rules_nixpkgs_dependencies")
rules_nixpkgs_dependencies()

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "nixos-unstable", # Any tag or commit hash
    sha256 = "" # optional sha to verify package integrity!
)

nixpkgs_package(
    name = "emacs-nox",
    repositories = { "nixpkgs": "@nixpkgs//:default.nix" }
)
