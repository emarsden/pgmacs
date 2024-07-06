# Prebuilt container image

If you want to get a quick feel for what PGmacs can do before installing it, you can try out our
prebuilt Podman/Docker container image and run PGmacs in terminal mode. It includes Emacs, PGmacs
and the necessary dependencies preinstalled.

    podman run --network host -ti ghcr.io/emarsden/pgmacs:latest

(It will also work with Docker if your prefer that to Podman.)

The container image is based on a lightweight Alpine Linux image and is around 150MB in size. It’s
built for the following platforms:

- Linux/AMD64
- Linux/Aarch64 (64-bit ARM)
- Linux/armv7 (32-bit ARM)
- Linux/riscv64

Note that you need to run the container with `--network host` or a similar commandline option that
allows network access to the PostgreSQL database.
