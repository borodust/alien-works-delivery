#
# MAIN
#
FROM clfoundation/sbcl:2.2.4-slim-bullseye

COPY appimagetool.sh /usr/local/bin/appimagetool
COPY build.sh /usr/local/bin/build.sh

RUN apt-get update \
    && apt-get -y install file curl fuse \
    && curl -L https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage --output /usr/local/bin/appimagetool.AppImage \
    && chmod +x /usr/local/bin/appimagetool.AppImage \
    /usr/local/bin/appimagetool \
    /usr/local/bin/build.sh \
    && groupadd -g 1001 builder \
    && useradd -m -u 1001 -g 1001 builder

USER builder:builder

ENTRYPOINT ["build.sh"]
