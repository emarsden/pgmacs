# Prebuilt container image

If you want safely to get a quick feel for what PGmacs can do before checking the source code and
installing it, you can try out our prebuilt Podman/Docker container image and run PGmacs in terminal
mode. It includes Emacs, PGmacs and the necessary dependencies preinstalled.

You will need either [Podman](https://podman.io/) (free software that’s available for Linux,
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




~~~admonish warning title="Security concerns regarding Emacs packages"

Any Emacs package that you install (whether from a package repository such as ELPA or MELPA, or via
`package-vc-install`) has full read/write/delete access to your data on your computer that is
accessible by the logged-in user, and (assuming no particular security protections are in place)
full network access to exfiltrate information. The 2024 [xz utils
backdoor](https://en.wikipedia.org/wiki/XZ_Utils_backdoor) incident illustrated the amount of effort
that malicious actors make to compromise our computers. Operating systems are introducing sandboxing
mechanisms for software installed from the internet to protect against these kinds of attacks, but
these protections are not effective for user-extensible software like Emacs. For this reason, it is
worthwhile spending some time reading the package’s source code to check for suspicious behaviours,
introduced by a malicious package developer or one whose computer has been compromised by an
attacker.

If you install Emacs packages via your operating system’s package distribution mechanism, the
package maintainers will — in the best case — play a gatekeeper role and check the source code on
behalf of users. Please note however that widely used package archives like MELPA are not equipped
to undertake any security checks before package updates are distributed to users. The Elpaca package
manager provides special [support for reviewing diffs](https://www.youtube.com/watch?v=5Ud-TE3iIQY)
to a package before installing them, which is definitely a good practice. Running packages in a
software container until you’ve been able to review them from a software security perspective, is a
partial response to some of these concerns.

~~~
