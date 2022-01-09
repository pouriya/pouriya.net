+++
title = "About Me"
+++



{{< figure class="avatar" src="/pouriya-jim-jahanbakhsh.png" alt="avatar">}}

{{< highlight xml "linenos=false" >}}
<note to="reader">
  <!-- Please keep in mind -->
  Number  1: I know what it is, and I'm not sure what it's not!
  Number  3: 1 + I know how to get things done (in a dirty way).
  Number  5: I know the basics but not the details.
  Number  7: I know it well, and I know where to find more details.
  Number 10: I know all the details, and I keep myself updated.
</note>
<!-- Never underestimate a man who overestimates himself -->
{{< / highlight >}}

## Operating systems
{{< highlight ini "linenos=false" >}}
[FreeBSD]
installation = 5
configuration = 3
internals = 3

[Linux]
installation = 6
configuration = 3
internals = 2

[Windows]
# TODO: Remove this section
installation = 3
configuration = 2
internals = 0
{{< / highlight >}}

## Protocols
{{< highlight json "linenos=false" >}}
{
    "HTTP": 6,
    "XMPP": 8,
    "MQTT": 7,
    "WebSocket": 6,
    "SMTP": 3,
    "LDAP": 3,
    "MATRIX": 3,
    "Signal": 4,
    "AMQP": 4,
    "CQL": 3
}
{{< / highlight >}}

## Programming Languages
{{< highlight yaml "linenos=false" >}}
erlang: 8
rust: 6
python: 5
go: 5
c: 4
ruby: 3
elixir: 3
awk: 3
lua: 2
perl: 2
javascript: 2
php: 2
{{< / highlight >}}

### Erlang
{{< highlight erlang "linenos=false" >}}
concepts() ->
    #{
        process => #{
            monitor => 7,
            link => 7,
            message_passing => 7
        },

        otp => #{
              application: 8,
              gen_server: 9,
              gen_fsm: 7,
              gen_event: 7,
              gen_statem: 6,
              supervisor: 8
        },

        hot_code_reloading => 7,
        distribution => 6,
        tracing => 6,
        meta_programming => 8,
        test => 5
    }.

tools() ->
    #{
        rebar3 => 7,
        mnesia => 6,

        ejabberd => #{ % A powerful XMPP & SIP & MQTT server
            internals => #{
                router => 8,
                database_management => 8,
                configuration => 7,
                plugin_management => 8,
                authentication_mechanism => 8
            },

            connection_manager => #{client => 9, server => 4, component => 7},

            develop_new_things => #{
                plugin => 9,
                authentication_mechanism => 8,
                database_backend => 7,
                new_configuration => 7
            },
            
            hooks => 8,
            clustering => 7
        },

        emqx => #{ % A distributed MQTT broker
            internals => 4,
            develop_new_plugins => 6
        },

        rabbitmq => #{ % A distributed AMQP & MQTT & STOMP broker
            internals => 2,
            develop_new_plugins => 4
        },

        http => #{
          cowboy => 6, elli => 5, % HTTP servers
          hackney => 6, ibrowse => 5, gun => 5 % HTTP Clients
        },

        brod => 7, % Apache Kafka client library
        lagger => 6,
        meck => 5, % Mock testing library
        'mysql-otp' => 5,
        epgsql => 4, % PostgreSQL client library
        escalus => 5, % XMPP client library
        marina => 6 % Cassandra CQL client library

        %% And many many more...
    }.

{{< / highlight >}}


### This page is in 'Work-In-Progress' state...
