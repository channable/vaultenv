{ pkgs ? (import ../../nix/nixpkgs-pinned.nix) {} }:

let
  vaultenv = (import ../../nix/release.nix).vaultenv;

  secretsFile = pkgs.writeTextFile {
    name = "test.secrets";
    destination = "/lib/test.secrets";
    text =
      ''
      data/testapp/config#username
      data/testapp/config#password
      '';
  };

  # TODO: Is this needed?
  hostsFile = pkgs.writeTextFile {
    name = "hosts";
    destination = "/etc/hosts";
    text = "";
  };

  # TODO: Is this needed?
  resolvConf = pkgs.writeTextFile {
    name = "resolv.conf";
    destination = "/etc/resolv.conf";
    text = "";
  };

  # When Vaultenv runs in the container in the pod, it needs to access Vault
  # that is running outside. When the pod is running in Minikube, Vaultenv can
  # reach the host network at host.minikube.internal. Its ip address is written
  # in /etc/hosts, which is mounted by Minikube inside the container. But we
  # still need to convince glibc to actually read /etc/hosts, and for that, we
  # need to provide an nsswitch config that has a "hosts: files" line.
  nsswitch = pkgs.writeTextFile {
    name = "nsswitch.conf";
    destination = "/etc/nsswitch.conf";
    text =
      ''
      # TODO: Are the non-hosts lines needed?
      passwd:         files
      group:          files
      shadow:         files
      gshadow:        files

      hosts:          files dns
      networks:       files

      protocols:      db files
      services:       db files
      ethers:         db files
      rpc:            db files

      netgroup:       nis
      '';
  };

  container = pkgs.dockerTools.buildLayeredImage {
    name = "vaultenv/vaultenv";
    tag = "test";
    contents = [
      vaultenv
      secretsFile
      hostsFile
      resolvConf
      nsswitch
      pkgs.coreutils
      pkgs.bash
      pkgs.iputils
    ];
  };

in
  container
