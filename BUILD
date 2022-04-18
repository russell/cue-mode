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

# genrule(
#     name = "emacs",
#     tools = [
#         "@emacs-nox//:bin",
#     ],
#     outs = ["output_file2"],
#     cmd = "$(locations @emacs-nox//:bin)/emacs > $(location output_file2)",
# )

# elisp_toolchain(
#     name = "hermetic",
#     emacs = ":emacs",
# )

elisp_test(
    name = "indent",
    srcs = ["tests/cue-mode-indent-tests.el"],
    deps = [":cue-mode"],
)
