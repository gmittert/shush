FROM haskell

RUN cabal update
ADD ./shush.cabal /opt/shush/shush.cabal
RUN cd /opt/shush && cabal install --only-dependencies -j5

ADD ./app /opt/shush/app
ADD ./src /opt/shush/src
ADD ./shush.conf /opt/shush/shush.conf
ADD ./LICENSE /opt/shush/LICENSE
RUN cd /opt/shush && cabal install

ENV PATH /root/.cabal/bin:$PATH

WORKDIR /opt/shush
EXPOSE 9001
CMD ["shush"]
