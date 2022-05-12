package main

import (
	"dagger.io/dagger"
	"dagger.io/dagger/core"
	"universe.dagger.io/docker"
)

dagger.#Plan & {
	actions: {
		image: docker.#Build & {
			steps: [
				docker.#Pull & {
					source: "index.docker.io/silex/emacs:28"
				},
			]
		}

		test: {
			src: core.#Source & {
				path: "."
			}

			run: docker.#Run & {
				_mountpoint: "/project"
				input:       image.output
				mounts: "Project files": {
					contents: src.output
					dest:     _mountpoint
				}

				workdir: _mountpoint
				command: {
					name: "emacs"
					args: [
						"-batch",
						"-L", _mountpoint,
						"-l", "ert",
						"-l", "hack/generate-autoloads.el",
						"-l", "tests/cue-mode-indent-tests.el",
						"-f", "ert-run-tests-batch-and-exit",
					]
				}
			}
		}
	}
}
