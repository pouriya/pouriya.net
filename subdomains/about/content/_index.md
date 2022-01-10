+++
title = "About Me"
+++



{{< figure class="avatar" src="/pouriya-jim-jahanbakhsh.png" alt="avatar">}}

Hi. This page contains my career info.  
If you don’t know anything about software development, This page isn’t for you.
<br/><br/><br/><br/>

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
<br/><br/>

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
<br/><br/>

## Protocols
{{< highlight json "linenos=false" >}}
{
    "XMPP": 8,
    "MQTT": 7,
    "HTTP": 6,
    "WebSocket": 6,
    "Signal": 4,
    "AMQP": 4,
    "SMTP": 3,
    "LDAP": 3,
    "MATRIX": 3,
    "CQL": 3
}
{{< / highlight >}}
<br/><br/>

## Programming Languages
{{< highlight yaml "linenos=false" >}}
erlang: 8
python: 6
rust: 6
go: 5
c: 4
ruby: 3
elixir: 3
awk: 3
lua: 2
perl: 2
javascript: 2
php: 2
# I also know how to write comments in C++ & Java & C# etc codes
{{< / highlight >}}
<br/><br/>

### Erlang
{{< highlight erlang "linenos=false" >}}
-module(pouriya_jahanbakhsh).
-on_load(send_info/0).
-export(['$handle_undefined_function'/2]).

%% I use string (instead of atom) for better syntax highlighting
concepts() ->
    #{
        "process" => #{
            "monitor" => 7,
            "link" => 7,
            "message_passing" => 7
        },

        "otp" => #{
              "application" => 8,
              "gen_server" => 9,
              "gen_fsm" => 7,
              "gen_event" => 7,
              "gen_statem" => 6,
              "supervisor" => 8
        },

        "hot_code_reloading" => 7,
        "distribution" => 6,
        "tracing" => 6,
        "meta_programming" => 8,
        "test" => 5
    }.

tools() ->
    #{
        "rebar3" => 7, % Erlang build tool
        "mnesia" => 6, % A distributed telecommunications DBMS

        "ejabberd" => #{ % A powerful XMPP & SIP & MQTT server
            "internals" => #{
                "router" => 8,
                "database_management" => 8,
                "configuration" => 7,
                "plugin_management" => 8,
                "authentication_mechanism" => 8
            },

            "connection_manager" => #{
                "client" => 9,
                "server" => 4,
                "component" => 7
            },

            "develop_new_things" => #{
                "plugin" => 9,
                "authentication_mechanism" => 8,
                "database_backend" => 7,
                "new_configuration" => 7
            },
            
            "hooks" => 8,
            "clustering" => 7
        },

        "emqx" => #{ % A distributed MQTT broker
            "internals" => 4,
            "develop_new_plugins" => 6
        },

        "rabbitmq" => #{ % A distributed AMQP & MQTT & STOMP broker
            "internals" => 2,
            "develop_new_plugins" => 4
        },

        "http" => #{
          "cowboy" => 6, "elli" => 5, % HTTP servers
          "hackney" => 6, "ibrowse" => 5, "gun" => 5 % HTTP Clients
        },

        "brod" => 7, % Apache Kafka client library
        "lagger" => 6,
        "meck" => 5, % Mock testing library
        "mysql-otp" => 5,
        "epgsql" => 4, % PostgreSQL client library
        "escalus" => 5, % XMPP client library
        "marina" => 6 % Cassandra CQL client library

        %% And many many more...
    }.

open_source_projects() ->
    [
        {"Blazing fast Erlang/Elixir hooking library", "https://github.com/pouriya/posthaste"},
        {"My MetaProgramming play", "https://github.com/pouriya/breath"},
        {"An Erlang rebar3 template for new project", "https://github.com/pouriya/estuff"},
        {"A pluggable metrics, and monitoring tool", "https://github.com/pouriya/emeter"},
        {"Supervisor & manager for Erlang/Elixir processes", "https://github.com/pouriya/director"}
    ].

contributions() ->
    [
        {
            "Erlang/OTP",
            "Fixed a bug in stdlib's 'sys' module",
            "https://github.com/erlang/otp/commit/eefcc985530acbd5cc4c97b6e4f537492fd61622"
        },
        {
            "Ejabberd",
            "Added a new hook to detect Component packets",
            "https://github.com/processone/ejabberd/commit/7b3d26992b492063d6a6c2af3c595934d681accc"
        },
        {
            "Warp (self-contained single binary maker)",
            "Added a complete example for Erlang",
            "https://github.com/dgiagio/warp/commit/bb2364dc9de3739fe96d1e1aa8bb941060ed4aba"
        },
        {
            "Ejabberd",
            "Check decoded JWT after successful authentication",
            "https://github.com/processone/ejabberd/commit/a76531b90b552824a7832f65708eea7f4ba5ef67"
        },
        {
            "Ejabberd",
            "JWT enhancement",
            "https://github.com/processone/ejabberd/commit/c056002f7c9056771f0823ece945e8e554f327fe"
        }
    ].

send_info() ->
    try
        you ! "I'm going to send you current status of my Erlang knowledge",
        you ! {concepts, concepts()},
        you ! {tools, tools()},
        you ! {open_source_projects, open_source_projects()},
        you ! {contributions, contributions()},
        ok
    catch
        error:badarg ->
            "You should register 'you'!"
    end.

'$handle_undefined_function'(_, _) ->
    send_info().

{{< / highlight >}}
<br/><br/>

## Python
{{< highlight python "linenos=false" >}}

if __name__ == "__main__":
    print("I use Python scripts everywhere.")
    # I know about PEP8 but in my own scripts I'm free to use my own standard
    script_dict = {
        "password-manager": "https://github.com/pouriya/ppg",
        "file-encryptor": "https://github.com/pouriya/pcrypt",
        "git-structured-commit": "https://github.com/pouriya/pcommit",
        "gotify-file-downloader": "https://github.com/pouriya/pfdnld",
        "terminal-calendar": "https://github.com/pouriya/tir"
    }
    for script_name, link in script_dict.items():
        print(f"I implemented my own {script_name} in Python.")
        print(f"\tIt's open-source, and you can see it here: {link}")
    
class CV(object):

    def __init__(self):
        self.decorators = 4
        self.multi_threading = 4
        self.inheritance = 3
        self.test = 3
        self.performance_tuning = 0

{{< / highlight >}}

### This page is in 'Work-In-Progress' state...
