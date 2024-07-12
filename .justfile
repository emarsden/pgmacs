# For use with the just command runner, https://just.systems/

default:
  @just --list


vhs:
  vhs etc/recording.tape

# Build a Podman/Docker container image for PGmacs
#
# Needs the package qemu-user-static installed to cross-build the various architectures.
container:
  podman manifest create pgmacs
  buildah build -f etc/Containerfile --platform linux/amd64 --tag pgmacs-linux-amd64 --manifest pgmacs .
  buildah build -f etc/Containerfile --platform linux/arm64 --tag pgmacs-linux-aarch64 --manifest pgmacs .
  buildah build -f etc/Containerfile --platform linux/arm/v7 --tag pgmacs-linux-armv7 --manifest pgmacs .
  buildah build -f etc/Containerfile --platform linux/riscv64 --tag pgmacs-linux-riscv64 --manifest pgmacs .


export INSTALL_EL := '''
    (message "Executing emacs-init")
    (unless (package-installed-p 'pg)
       (package-vc-install "https://github.com/emarsden/pg-el" nil nil 'pg))
    (unless (package-installed-p 'pgmacs)
       (package-vc-install "https://github.com/emarsden/pgmacs"))

    (require 'pgmacs)
'''
tmpdir := `mktemp -d`
init-el := tmpdir / "init.el"

# Check whether our package-vc-install instructions work on a pristine install.
installability:
   printf '%s' "$INSTALL_EL" > {{ init-el }}
   ls -l {{ init-el }}
   cat {{ init-el }}
   podman run --rm -ti -v {{ tmpdir }}:/tmp docker.io/silex/emacs:29.4-ci \
      ${EMACS:-emacs} -l /tmp/init.el


list-docker-platforms:
    podman run --rm docker.io/mplatform/mquery ghcr.io/emarsden/pgmacs:latest


# Run a trivy vulnerability scan of our container image
# https://github.com/aquasecurity/trivy
trivy-container:
    podman run --rm --pull=newer docker.io/aquasec/trivy image ghcr.io/emarsden/pgmacs:latest

trivy-repository:
    podman run --rm --pull=newer -v $PWD:/myapp docker.io/aquasec/trivy fs --scanners vuln,secret,misconfig .


# Run a grype vulnerability scan of our container image
# https://github.com/anchore/grype
grype-container:
    podman run --rm -it docker.io/anchore/grype ghcr.io/emarsden/pgmacs:latest
