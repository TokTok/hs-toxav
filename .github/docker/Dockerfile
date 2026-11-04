FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-toxav"]
COPY --chown=builder:users . /work/hs-toxav
RUN ["stack", "install", "toxav"]

FROM scratch
COPY --from=build /home/builder/.local/ /
