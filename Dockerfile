FROM jupyter/minimal-notebook:59904dd7776a

USER root

# Erlang
# Install Erlang Solutions repository
RUN apt-get update && \
    apt-get install -y gnupg2 && \
    wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb && \
    rm erlang-solutions_1.0_all.deb && \
    apt-get install -y apt-utils && \
    apt-get install -y curl && \
    apt-get install -y erlang && \
    apt-get install -y elixir


USER $NB_USER

# Erlang, LFE and Elixir
RUN git clone --depth 1 https://github.com/filmor/ierl.git && \
    cd ierl && \
    ./_download_rebar3.sh && ./rebar3 compile && \
    mkdir /home/$NB_USER/.ierl && \
    cp _build/default/bin/ierl /home/$NB_USER/.ierl/ierl.escript && \
    chmod +x /home/$NB_USER/.ierl/ierl.escript && \
    /home/$NB_USER/.ierl/ierl.escript install erlang --user && \
    /home/$NB_USER/.ierl/ierl.escript install lfe --user && \
    /home/$NB_USER/.ierl/ierl.escript install elixir --user && \
    cd .. && \
    rm -rf ierl
