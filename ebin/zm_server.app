{application,  zm_server,
[
{description, "Distributed message broker server."},
{vsn, "0.1.0"},
{modules, [
            zm_app,
            zm_sup,
            zm_channel_sup,
            zm_filter_channel,
            zm_channel
          ]},
{registered, [zm_sup, zm_channel_sup]},
{applications, [kernel,stdlib,sasl,mnesia]},
{mod, {zm_app, []}}
]}.
