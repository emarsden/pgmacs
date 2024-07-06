# For use with the just command runner, https://just.systems/

default:
  @just --list


vhs:
  vhs etc/recording.tape

# Build a Podman/Docker container image for PGmacs
container:
  podman manifest create pgmacs
  buildah build -f etc/Containerfile --platform linux/amd64 --tag pgmacs-linux-amd64 --manifest pgmacs .
  buildah build -f etc/Containerfile --platform linux/arm64 --tag pgmacs-linux-aarch64 --manifest pgmacs .
  buildah build -f etc/Containerfile --platform linux/arm/v7 --tag pgmacs-linux-armv7 --manifest pgmacs .
  buildah build -f etc/Containerfile --platform linux/riscv64 --tag pgmacs-linux-riscv64 --manifest pgmacs .
