workspace(name = "cue-mode")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Elisp

http_archive(
    name = "phst_rules_elisp",
    sha256 = "8754266deb5166671d02729457cba167d56bb546fc84e74919f474ebd58b1345",
    strip_prefix = "rules_elisp-743433f5bfd98e611b061f987c412b5ea6622ba9",
    urls = [
        "https://github.com/phst/rules_elisp/archive/743433f5bfd98e611b061f987c412b5ea6622ba9.zip",  # 2021-05-16
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
