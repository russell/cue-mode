load(
    "@phst_rules_elisp//elisp:defs.bzl",
    "elisp_binary",
    "elisp_library",
    "elisp_test",
    "elisp_toolchain",
)

elisp_library(
    name = "cue-mode",
    srcs = ["cue-mode.el"],
    # load_path = ["."],
)

elisp_test(
    name = "indent",
    srcs = ["tests/cue-mode-indent-tests.el"],
    deps = [":cue-mode"],
)

elisp_binary(
    name = "generate-autoloads",
    src = "hack/generate-autoloads.el",
)
