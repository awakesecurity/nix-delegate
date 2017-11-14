# `nix-delegate`

This is a command-line utility that you can use to run a subcommand with
distributed builds transiently enabled:

```bash
$ nix-delegate --help
Usage: nix-delegate --host ARG ([--x86_64-linux] | [--x86_64-darwin])
                    [--key ARG] [--cores ARG] [--feature ARG] COMMAND [ARGS]
  Run a subcommand with distributed builds transiently enabled

Available options:
  --host ARG               Machine to use as a build slave
  --key ARG                Path to SSH private key (Default: ~/.ssh/id_rsa)
  --cores ARG              Number of cores to use (Default: 1)
  --feature ARG            Supported system features
  COMMAND                  Command to delegate (if 'nix-build', sudo will be
                           used if $NIX_REMOTE=daemon)
  -h,--help                Show this help text
```

The first positional argument is the command to run with distributed builds
transiently enabled (in this example, `nix-build`). All options, flags, or
arguments immediately following the first positional argument are collected and
fed to the command.

Example:

```bash
$ nix-delegate --host parnell@jenkins-slave-nixos01 --cores 4 --x86_64-linux --key /home/parnell/.ssh/awake nix-build --no-out-link -A shipit release.nix
[+] Downloading: /etc/nix/signing-key.sec
[+] Installing: /etc/nix/signing-key.sec
[+] Downloading: /etc/nix/signing-key.pub
[+] Installing: /etc/nix/signing-key.pub
[+] Running command: sudo nix-build --no-out-link -A shipit release.nix
[+] Full command context: sudo NIX_BUILD_HOOK=/nix/store/jj3kq2dmllvkqwwbhnmzbk9hfgncdbvl-nix-1.11.6/libexec/nix/build-remote.pl
...
/nix/store/wwrrkwzhq43c22if31d65qikslmvivyc-shipit-1.0.0
```

If you are on a system with the nix-daemon running and you are not root, this
utility will detect if your `nix-build` work is handled by the `nix-daemon` and
if `nix-build` is supplied in the first positional argument then `sudo` will be
automatically prepended to the command.

Any other commands supplied are not auto-prefixed with `sudo`.
