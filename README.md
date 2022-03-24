amoc_arsenal_xmpp
=====

This is a collection of XMPP-related scenarios for [amoc](https://github.com/esl/amoc), that [Erlang Solutions](https://www.erlang-solutions.com/) uses to load test [MongooseIM](https://github.com/esl/MongooseIM) - our robust, scalable and efficient XMPP server.


docker exec -it amoc-arsenal-xmpp-amoc-master-1 /home/amoc/amoc_arsenal_xmpp/bin/amoc_arsenal_xmpp remote_console

amoc_dist:do(slash_one_to_one, 2, []).
amoc_dist:do(mongoose_one_to_one, 2, [{mim_host, <<"prod.xmpp.hiapp-chat.com">>}]).
amoc_dist:start(mongoose_one_to_one,0,[{mim_host, <<"prod.xmpp.hiapp-chat.com">>}]).
amoc_dist:do(pubsub_simple, 2, [{mim_host, <<"prod.xmpp.hiapp-chat.com">>}, {pubsub_addr, <<"pubsub.prod.xmpp.hiapp-chat.com">>}]).
amoc_dist:add(50).

docker run -d --network amoc-test-network\
 --name=graphite\
 --restart=always\
 -p 80:80\
 -p 2003-2004:2003-2004\
 -p 2023-2024:2023-2024\
 -p 8125:8125/udp\
 -p 8126:8126\
 graphiteapp/graphite-statsd

 docker run --rm -d --name=graphite --network amoc-test-network \
    -p 2003:2003 -p 8080:80 graphiteapp/graphite-statsd