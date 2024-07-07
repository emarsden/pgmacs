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
