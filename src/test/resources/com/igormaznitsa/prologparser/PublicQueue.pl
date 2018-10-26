start(X) :-	msg_queue_create('MyQueue'), 
			thread_create(ID, child(X)), 
			thread_sleep(500), 
			thread_send_msg('MyQueue', m('important message')), 
			thread_join(ID, child(X)).
			
child(X) :- thread_get_msg('MyQueue', m(X)).