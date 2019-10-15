#!/bin/sh

if command -v csi; then
    cd $(csi -R chicken.platform -p '(chicken-home)')
    curl http://3e8.org/pub/chicken-doc/chicken-doc-repo-5.tgz | tar -xz
fi
