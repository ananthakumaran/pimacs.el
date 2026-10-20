FROM node:22.22.2-bookworm-slim

ARG PI_VERSION=1.1.0

RUN apt-get update \
    && apt-get install --yes --no-install-recommends openssh-server jq ripgrep \
    && rm -rf /var/lib/apt/lists/* \
    && rm -f /etc/ssh/ssh_host_* \
    && npm install --global --ignore-scripts --prefix /opt/pimacs-test \
       "@earendil-works/pi-coding-agent@${PI_VERSION}" \
    && npm cache clean --force \
    && useradd --create-home --shell /bin/bash pimacs \
    && printf 'pimacs:pimacs\n' | chpasswd \
    && install -d /run/sshd \
    && install -d -o pimacs -g pimacs /srv/pimacs-tramp/project \
    && printf '%s\n' 'This file exists only on the TRAMP test server.' \
       > /srv/pimacs-tramp/project/remote-only.txt \
    && chown pimacs:pimacs /srv/pimacs-tramp/project/remote-only.txt

RUN printf '%s\n' \
    'HostKey /etc/ssh/ssh_host_ed25519_key' \
    'PidFile /run/sshd.pid' \
    'PubkeyAuthentication no' \
    'AllowUsers pimacs' \
    'PermitRootLogin no' \
    'PasswordAuthentication yes' \
    'PermitEmptyPasswords no' \
    'KbdInteractiveAuthentication no' \
    'UsePAM no' \
    'AllowAgentForwarding no' \
    'AllowTcpForwarding no' \
    'X11Forwarding no' \
    'Subsystem sftp internal-sftp' \
    > /etc/ssh/sshd_config

WORKDIR /srv/pimacs-tramp/project

CMD ["sh", "-ec", "ssh-keygen -A; exec /usr/sbin/sshd -D -e"]
