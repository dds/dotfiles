#!/bin/sh
export DEBIAN_FRONTEND=noninteractive

### Add external package sources
add-apt-repository --yes --no-update ppa:kelleyk/emacs
curl -sL https://deb.nodesource.com/setup_10.x | sh -x

### Update packages
apt-get update

### Remove conflicting packages
apt-get remove -y --purge \
  emacs25-common

### Bring the whole system up to date
apt-get -u -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" dist-upgrade

### Install extra packages
apt-get install -y \
  emacs26 \
  gdm3 \
  gnome-session \
  gnome-terminal \
  isync \
  python3-keyring \
  nodejs

### Configure
sudo -u vagrant git checkout /dotfiles ~vagrant/dotfiles
sudo -u vagrant ~vagrant/dotfiles/install
