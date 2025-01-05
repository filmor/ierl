FROM jupyter/minimal-notebook:e7000ca1416d

USER root

# Erlang
# Install Erlang Solutions repository
RUN apt-get update && \
    apt-get install -y gnupg2 curl apt-utils && \
    wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb && \
    rm erlang-solutions_1.0_all.deb && \
    apt-get update && \
    apt-get install -y erlang

COPY / ierl_repo
RUN chown $NB_USER -R ierl_repo

USER $NB_USER

RUN git clone ierl_repo ierl && \
    cd ierl && \
    mkdir /home/$NB_USER/.ierl && \
    ./_download_rebar3.sh && \
    ./rebar3 escriptize && \
    cp _build/default/bin/ierl /home/$NB_USER/.ierl/ierl.escript && \
    chmod +x /home/$NB_USER/.ierl/ierl.escript && \
    /home/$NB_USER/.ierl/ierl.escript install erlang --user && \
    /home/$NB_USER/.ierl/ierl.escript install lfe --user && \
    /home/$NB_USER/.ierl/ierl.escript install elixir --user && \
    cd .. && \
    rm -rf ierl ierl_repo
