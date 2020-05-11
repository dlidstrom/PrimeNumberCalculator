FROM gitpod/workspace-dotnet

USER gitpod

RUN git clone https://github.com/MichaelMure/git-bug.git
RUN cd /home/gitpod/git-bug && make
