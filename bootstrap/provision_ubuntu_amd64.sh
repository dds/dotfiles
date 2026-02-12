#!/bin/sh
USER=${USER:-vagrant}
SRCDIR=${SRCDIR:-$(dirname "`pwd`")}  # parent directory should be root
TARGET=${TARGET:-$(getent passwd "$USER" | cut -d: -f6)}

### Set environment variables
export DEBIAN_FRONTEND=noninteractive

### Add external package sources
add-apt-repository --yes --no-update ppa:yubico/stable

### Update packages
apt-get update

### Install packages
apt-get install -y --ignore-missing \
  build-essential \
  curl \
  emacs \
  gdm3 \
  gnome-session \
  gnome-terminal \
  gpg \
  gpg-agent \
  isync \
  keychain \
  msmtp \
  nodejs \
  npm \
  pipx \
  python3 \
  python3-pip \
  python3-jinja2 \
  python3-keyring \
  ripgrep \
  ruby \
  screen \
  syncthing \
  tree \
  vim \
  x11-xkb-utils \
  workrave \
  yubikey-personalization-gui \
  python3-yubikey-manager \
  libnss-resolve

### Bring the whole system up to date
apt-get -u -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade

### Install Ruby tools
gem install rake

### Configure
systemctl restart gdm3

### Install dotfiles
sudo -u "$USER" rake -f "${SRCDIR}/Rakefile" install["$TARGET"]
