{application,metalkia_xmpp,
             [{description,"Metalkia XMPP interface"},
              {vsn,"0.1"},
              {registered,[]},
              {applications,[kernel,stdlib,exmpp,metalkia_riak]},
              {mod,{mtxmpp_app,[]}},
              {env,[]},
              {modules,[mtxmpp_app,mtxmpp_sup]}]}.