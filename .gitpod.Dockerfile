FROM gitpod/workspace-dotnet

USER gitpod

# Install custom tools, runtime, etc. using apt-get
# For example, the command below would install "bastet" - a command line tetris clone:
#
# RUN sudo apt-get -q update &&
#     sudo apt-get install -yq bastet &&
#     sudo rm -rf /var/lib/apt/lists/*
#
# More information: https://www.gitpod.io/docs/config-docker/
# FROM gitpod/workspace-full:latest

# USER root

#RUN wget -q https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb && \
#    dpkg -i packages-microsoft-prod.deb && rm -rf packages-microsoft-prod.deb && \
#    add-apt-repository universe && \
#    apt-get update && apt-get -y -o APT::Install-Suggests="true" install dotnet-sdk-2.2 && \
#    apt -y clean;
# RUN sudo apt-get -q update &&
#    sudo apt-get install -yq 
RUN git clone https://github.com/MichaelMure/git-bug.git
# RUN cd git-bug && make install
