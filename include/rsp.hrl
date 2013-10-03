%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Rock-scissors-paper game.
%%%
%%% Created : Oct 1, 2013
%%% -------------------------------------------------------------------
-record(rsp_match_tb, {id='_',
					   ref='_',
					   player1_id='_',
					   player2_id='_',
					   player1_seq='_',
					   player2_seq='_',
					   start_date='_',
					   start_time='_',
					   end_date='_',
                       end_time='_',
                       event_id='_'}).

-record(rsp_event_tb, {id='_',
                       name='_',
                       ref='_',
                       start_date='_',
                       start_time='_',
                       end_date='_',
                       end_time='_'}).

