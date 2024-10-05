# Prebuilt container image

If you want to get a quick feel for what PGmacs can do before installing it, you can try out our
prebuilt Podman/Docker container image and run PGmacs in terminal mode. It includes Emacs, PGmacs
and the necessary dependencies preinstalled.

You will need either [Podman](http://podman.io/) (free software that’s available for Linux,
Microsoft Windows and MacOS) or Docker installed.

    podman run --network host -ti ghcr.io/emarsden/pgmacs:latest

[![Container size](https://ghcr-badge.egpl.dev/emarsden/pgmacs/size?label=Container%20image)](https://github.com/users/emarsden/packages/container/package/pgmacs)

The container image is based on a lightweight Alpine Linux image, and is rebuilt for
each new PGmacs release. It’s built for the following platforms:

- Linux/AMD64
- Linux/Aarch64 (64-bit ARM)
- Linux/armv7 (32-bit ARM)
- Linux/riscv64


Note that you need to run the container with `--network host` or a similar commandline option that
allows **network access to the PostgreSQL database**. Since this container runs Emacs in terminal
mode, rather than GUI mode, some PGmacs functionality is not available (in particular, the [SchemaSpy
support](schemaspy.html)).
