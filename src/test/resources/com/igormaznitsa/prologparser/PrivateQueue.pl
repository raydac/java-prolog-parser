start(X) :-	thread_create(ID1, thread1(ID1)), 
			thread_sleep(200), 
			thread_create(ID2, thread2(ID1)), 
			thread_create(ID3, thread3(ID1,X)), 
			thread_read(ID3, _), 
			write('Father: done').
			
thread1(ID) :- 	tell('threadLog.txt'), 
				write('ID1: waiting for message'), 
				thread_get_msg(ID, m(X)), 
				write('ID1: message retrieved').
				
thread2(ID) :- 	thread_send_msg(ID, m('critical message')), 
				write('ID2: message sent').
				
thread3(ID,X):- thread_peek_msg(ID, m(X)), 
				write('ID3: trying to get message').