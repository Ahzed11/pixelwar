-module(pixelwar_sup).

-behaviour(supervisor).
-export([start_link/0, add_matrix/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_matrix(ChannelName) ->
    supervisor:start_child(?MODULE, [ChannelName]).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecifications = [
        #{
            id => pixelwar_matrix_serv,
            start => {pixelwar_matrix_serv, start_link, []},
            restart => transient,
            shutdown => 2000,
            type => worker,
            modules => [pixelwar_matrix_serv]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
