FROM gitpod/workspace-dotnet

USER gitpod

RUN git clone https://github.com/MichaelMure/git-bug.git && \
    cd git-bug && \
    make
