(require 'package)

(package-generate-autoloads "cue-mode" (or (getenv "BUILD_WORKSPACE_DIRECTORY")
                                           (getenv "PWD")
                                           "."))
