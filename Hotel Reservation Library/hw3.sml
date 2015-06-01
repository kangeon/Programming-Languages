(*Geon Kang
  ID: N17120399 
  Programming Languages - Assignment 3
  Fall 2013*)
  
(* signature of a structure containing quantity of each room configuration,
	minimum night requirement, and room occupancy limit 
	(Since these values should be fixed for a given hotel) passed to functor *)
signature ROOMDETAIL =
	sig
		val doubleAvailable : int
		val kingAvailable : int
		val queenAvailable : int
		val minnights : int option
		val occupancyLimit : int
	end;

(*Functor MakeHotel creates a hotel reservation structure given a ROOMDETAIL structure*)
(* Using opaque ascription *)
functor MakeHotel (structure Q : ROOMDETAIL) :>
sig
	(* datatype to represent room configuration*)
	datatype roomconfig = Double | Queen | King
	(* reservation record represented by a tuple.
	 the fields are: 
	 (ID number, First Name, Last Name, Date of check-in, number of nights, number of guests, type of room)
		(* It is assumed that the date of check-in and number of nights is a non-negative integer.
		However, this restriction is not enforced, as it was not specified to raise an exception in such case.
		It will probably be a good idea to deal with such cases moving forward.
		*)
	   (*In retrospect, a tuple was a poor design choice because it is hard to keep track
		of what field you are accessing. (since you access field with numbers)
		A record would have been a better choice since we can access fields with identifiers
		that make it easier to keep track of what field we are accessing
		*)
	*)
	type resrecord = int * string * string * int * int * int * roomconfig
	(* reservation system. Not exposed to library user *)
	type ressys 
	
	(* declare exceptions *)
	exception NotEnoughRooms
	exception SameIdExists
	exception IdNotFound
	exception DoesntMeetRestrictions
	
	(* declare signatures of functions 
	(*exception of empty: empty is not a function because it takes no parameters*)*)
	val empty : ressys
	val getInventory : ressys -> roomconfig -> int -> int
	val getInventorySpan : ressys -> roomconfig -> int -> int -> bool
	(*earliesetAvailaible is my custom addition to the reservation system*)
	val earliestAvailable : ressys -> roomconfig -> int -> int -> int
	val completedStays : ressys -> int -> int
	val removeCompletedStays : ressys -> int -> ressys
	val guestQuantity : ressys -> int -> int
	val reserve : ressys -> resrecord -> ressys
	val cancel : ressys -> int -> ressys

end = struct
	datatype roomconfig = Double | Queen | King
	type resrecord = int * string * string * int * int * int * roomconfig
	(* reservation system represented by tuple. only defined in structure to not expose to library user *)
	type ressys = int * int * int * resrecord list
	
	exception NotEnoughRooms
	exception SameIdExists
	exception IdNotFound
	exception DoesntMeetRestrictions
	
	(*define functions*)
	
	(*creates an empty reservation system with number of room configs available given by Q of ROOMDETAIL*)
	val empty = ((Q.doubleAvailable, Q.queenAvailable, Q.kingAvailable, []):ressys)

	(*Given a reservation system, room configuration, and date, 
	getInventory returns the number of rooms of the specified configuration available for the given date.
	(*Uses helper function countfull:
	helper function countfull takes a reservation record list and a date, 
	and returns the number of occupied rooms of the specified configuration for the given date*) 
	getInventory subtracts the return value of countfull from the total number of rooms of the specified configuration it has,
	and returns the value*)
	fun getInventory (x:ressys) (y:roomconfig) (z:int) =
		let fun countfull nil (y:roomconfig) (z:int) = 0
		    | countfull ((x::xs):resrecord list) y z =
				if (#7(x)) = y andalso (#4(x)) <= z andalso ((#4(x))+(#5(x))) > z then 1 + (countfull xs y z) 
				else (countfull xs y z)
		in
			if y=Double then (#1(x))-(countfull (#4(x)) y z)
			else if y=Queen then (#2(x))-(countfull (#4(x)) y z)
			else (#3(x))-(countfull (#4(x)) y z)
		end

	(*Given a reservation system, room configuration, check-in date, and number of nights,
	getInventorySpan checks whether a room of the specified configuration is available for the span of the entire period of time. 
	(*Uses getInventory to see if the inventory is not 0 for each date in the specified period*)
	If the inventory is greater than 0 for the entire period returns true
	else returns false*)
	fun getInventorySpan (x:ressys) (y:roomconfig) (z:int) 0 = true
	| getInventorySpan (x:ressys) y z a =
		if (getInventory x y z) >0 then (getInventorySpan x y (z+1) (a-1))
		else false

	(*Given a reservation system and date, completedStays counts the number of completed stays before the given date 	
	(*uses helper function countCompleted that takes a reservation record list and a date,
	recurses through list keeping a count of the stays that are completed before the given date,
	and returns the count*)
	The point of the helper function is to deal with just the resrecord list provided in the reservation system
	rather than trying to directly deal with a tuple field access.*)
	fun completedStays (x:ressys) (y:int) =
		let fun countCompleted nil y = 0
		    | countCompleted ((x::xs):resrecord list) y =
				if ((#4(x))+(#5(x))-1) < y then 1 + (countCompleted xs y)
				else (countCompleted xs y)
		in
			(countCompleted (#4(x)) y)
		end

	(*Given a reservation system and a date, 
	removeCompletedStays removes stays completed by the specified date from the given reservation system.
	(*uses helper function removeCompleted:
	removeCompleted takes a reservation record list and a date, 
	recurses through list and takes out reservation records that have been completed by the specified date,
	and returns the resulting resrecord list*)
	The point of the helper function is to deal with just the resrecord list provided in the reservation system
	rather than trying to directly deal with a tuple field access.*)
	fun removeCompletedStays (x:ressys) (y:int) =
		let fun removeCompleted nil y = []
		    | removeCompleted ((x::xs):resrecord list) y =
				if ((#4(x))+(#5(x))-1) < y then (removeCompleted xs y) 
				else x::(removeCompleted xs y)
		in
			(#1(x),#2(x),#3(x),(removeCompleted (#4(x)) y)):ressys
		end

	(*Given a reservation system and a date,
	guestQuantity returns the total number of guests staying at the hotel at the specified date.
	(*uses helper function guestCounter:
	guestCounter takes a resrecord list and a date
	and recurses through the list, checking if the specified date is within the range of the stay for each resrecord.
	It returns the total sum of the number of guests for all such resrecords in the resrecord list*)
	The point of the helper function is to deal with just the resrecord list provided in the reservation system
	rather than trying to directly deal with a tuple field access.*)
	fun guestQuantity (x:ressys) (y:int) =
		let fun guestCounter nil (y:int) = 0
		    | guestCounter ((x::xs):resrecord list) y = 
				if (#4(x))-1 < y andalso((#4(x))+(#5(x)))> y then #6(x) + (guestCounter xs y)
				else (guestCounter xs y)
		in 
			(guestCounter (#4(x)) y)
		end
	
	(* (*Custom Addition to the reservation System *) 
	Given a reservation system, roomconfig, date, and number of nights,
	earliestAvailable returns the earliest date that the specified roomconfig is available
	for the given number of nights
	Uses getInventorySpan to check if the stay is available starting at the date specified
	if the stay is not available at the date, the function continues to recursively check the next day.
	earliestAvailable will return the earliest date starting from the specified date that getInventorySpan returns true*)
	fun earliestAvailable (x:ressys) (y:roomconfig) (z:int) (a:int) =
		if (getInventorySpan x y z a) then z
		else (earliestAvailable x y (z+1) a)	
	(*(*helper function*) Given a resrecord list and an ID,
	findid recurses through the list, checking if the first field of each resrecord equals the specified ID.
	if the ID is found, returns true
	else returns false *)
	(* defined outside of let because it used by two functions: reserve & cancel. 
	   only placed before reserve & cancel because they are the only two functions that use findid *)
	fun findid nil (y:int) = false
	| findid ((x::xs):resrecord list) y = 
		if (#1(x)) = y then true
		else (findid xs y)

	(*Given a reservation system and a reservation record,
	reserve adds the reservation record to the resrecord list in the system.
	(*Uses helper function restrictions:
	restrictions takes a reservation system and a reservation record
	and checks if the number of nights and number of guests are within the limits as set by ROOMDETAIL
	if there is no minimum night requirement specified and the number of guests is less than or equal to the occupancy limit, return true
	if the number of nights is less than the min night requirement or the number of guests is greater than the occupancy limit, return false
	else return true*)
	(*Uses getInventorySpan to check if the specified room is available for the given time span. If it is not, raise exception *)
	(*Uses helper function findid to check if a reservation with the specified ID already exists in the system. If there is, raise exception*)
	If the given resrecord passes all the above checks, add the resrecord to the FRONT of the resrecord list in the system, and return the system*)
	fun reserve (x:ressys) (y:resrecord) =  
		let fun restrictions (x:resrecord) =
			if not(isSome(Q.minnights)) andalso ((#6(x)) <= Q.occupancyLimit) then true
			else if ((#5(x)) < (valOf(Q.minnights))) orelse ((#6(x)) > Q.occupancyLimit) 
				then false
			else true
		in
			if (getInventorySpan x (#7(y)) (#4(y)) (#5(y)))=false then raise NotEnoughRooms
			else if (findid (#4(x)) (#1(y))) then raise SameIdExists
			else if not(restrictions y) then raise DoesntMeetRestrictions
			else (#1(x),#2(x),#3(x),y::(#4(x))):ressys
		end

	(*Given a reservation system and an ID,
	cancel removes a reservation record with the specified ID in the resrecord list in the system
	findid is used to check if a resrecord with the given ID does exist in the system. 
	If such a resrecord does not exist, raise an exception.
	(*uses helper functions deleterec:
	deleterec takes a resrecord list and an ID and recurses through the list to see if the first field of each resrecord equals the specified ID.
	it removes the resrecord with the ID and returns the rest of the list*)
	We do not have to worry about deleterec not finding a resrecord with the given ID because we call findid first and raise an exception in that case*)
	(*The point of the helper function deleterec is to deal with just the resrecord list provided in the reservation system
	rather than trying to directly deal with a tuple field access.*)
	fun cancel (x:ressys) (y:int) =
		let fun deleterec nil y = []
		    | deleterec ((x::xs):resrecord list) y =
				if (#1(x)) = y then xs
				else x::(deleterec xs y)
		in
			if not(findid (#4(x)) y) then raise IdNotFound
			else (#1(x),#2(x),#3(x),(deleterec (#4(x)) y)):ressys
		end
		
end;
(*==========================Begin Test bed=========================================================*)

(* 1. *)
(*Instantiate structure adhering to ROOMDETAIL signature. *)
(*This structure has 2 double rooms, 3 queen rooms, 4 king rooms,
an occupancy limit of 4 persons per room and a minimum night requirement of 2 nights*) 
structure HiltonRoomDetail = 
struct
	val doubleAvailable = 2
	val queenAvailable = 3
	val kingAvailable = 4
	val minnights = SOME 2
	val occupancyLimit = 4
end;

(* 2. ------------------------------------------------------------------------------- *)
(*Instatiate one hotel reservation structure using the functor MakeHotel,
passing HiltonRoomDetail from the previous step *)
structure HiltonSys = MakeHotel(structure Q = HiltonRoomDetail);

(* open HiltonSys (*just for the purposes of reducing things to type*)*)
open HiltonSys;

(* start with empty reservation system *)
val sys = empty;

(* 3. ------------------------------------------------------------------------------ *)
(* Add five reservations:
expect all of them to be added successfully*)

val sys = reserve sys ((1,"Charles","Babbage",1,2,1,Double):resrecord);
val sys = reserve sys ((2,"Alan","Turing",2,3,2,Queen):resrecord);
val sys = reserve sys ((3,"Tommy","Flowers",3,4,3,King):resrecord);
val sys = reserve sys ((4,"John", "von Neumann",4,5,4,Queen):resrecord);
val sys = reserve sys ((5,"Douglas", "Engelbart", 5,6,3,Queen):resrecord);

(* 4. ------------------------------------------------------------------------------ *)
(*show unsuccessful creation of a reservation due to lack of Double rooms.
Attempt to book two more double rooms with overlapping stay periods.
One Double room is already occupied by reservation #1 for days 1 to 2.
There is still one Double room available, so reservation #6 (days 1 to 3) is successfully reserved.
However, reservation #7 (days 1 to 5) will fail because all the doubles are taken for days 1 and 2. *)
(* handled exceptions to prevent program from terminating *)

val sys = reserve sys ((6,"Harold", "Abelson", 1,3,2,Double):resrecord);
val sys = reserve sys ((7,"Vinton", "Cerf", 1,5,3,Double):resrecord)
		handle
		NotEnoughRooms => (print "Not Enough Rooms Available \n"; sys)
		| SameIdExists => (print "Same ID Already Exists \n"; sys)
		| DoesntMeetRestrictions => (print "Doesn't Meet Restrictions \n"; sys);

(* 5. ----------------------------------------------------------------------------- *)
(* The following two reservations will fail: 
reservation #8 will fail due to room occupancy limit being exceeded 
(trying to put 5 people in a room when limit for a room is 4),
reservation #9 will fail due to minimum night requirement not being met
(trying to stay 1 night when the minimum night requirement is 2*)

val sys = reserve sys ((8,"John","Backus",6,7,5,King):resrecord)
		handle
		NotEnoughRooms => (print "Not Enough Rooms Available \n"; sys)
		| SameIdExists => (print "Same ID Already Exists \n"; sys)
		| DoesntMeetRestrictions => (print "Doesn't Meet Restrictions \n"; sys);
val sys = reserve sys ((9,"Alonzo","Church",7,1,2,Queen):resrecord)
		handle
		NotEnoughRooms => (print "Not Enough Rooms Available \n"; sys)
		| SameIdExists => (print "Same ID Already Exists \n"; sys)
		| DoesntMeetRestrictions => (print "Doesn't Meet Restrictions \n"; sys);

(* 6. ---------------------------------------------------------------------------- *)
(* Cancelling reservations with ID 1 and ID 2*)
val sys = cancel sys 1;
val sys = cancel sys 2;

(* 7. *)
(* Query room inventory of Queen rooms for day 7:
reservation #4 (days 4 to 8) and reservation #5 (days 5 to 10) are using Queen rooms at this date,
There should be 3 - 2 = 1 more Queen room available on day 7.
so expect an output of 1 *)
getInventory sys Queen 7;

(* 8. ----------------------------------------------------------------------------*)
(* Demonstrate removeCompletedStays and completedStays:
First call completed stays with date 7. By date 7, reservation #3 (days 3 to 6) 
and reservation #6 (days 1 to 3) should be completed
so expect an output of 2 *)
completedStays sys 7;

(* remove completed stays by date 7:
reservation #3 and reservation #6 should be gone from the system
leaving only reservation #4 and #5 *)
val sys = removeCompletedStays sys 7;

(* call check completed stays by date 7 again 
should now evaluate to 0*)
completedStays sys 7;

(* 9. --------------------------------------------------------------------------- *)
(*Demonstrate guestQuantity function*)
(*Check guest quantity for date 7:
should be (number of guests for reservation #4) + (number of guests for reservation #5)
That is 4 + 3
so output should be 7 *)
guestQuantity sys 7;

(*Check guest quantity for date 9:
should be (number of guests for reservation #5) because reservation #4 completes on the night of date 8.
so output should be 3 *)
guestQuantity sys 9;

(* 10. ---------------------------------------------------------------------------- *)
(*Demonstrate custom new feature*)
(*test earliestAvailable which, given a reservation system, roomconfig, date, and number of nights,
returns the earliest date from the specified date on that a room of the given roomconfig is available for the given number of nights*)

(*Check earliest Queen room availability for 10 nights starting from date 7. Reservation #4 is from day 4 to 8
and Reservation #5 is from day 5 to 10, so on day 7, two Queen rooms are taken. Since there is
one Queen room still available on day 7, output should be 7*)
earliestAvailable sys Queen 7 10;

(*make a new reservation to make Queen rooms unavailable for day 7. *)
val sys = reserve sys ((10, "James","Gosling",7,4,1,Queen):resrecord);

(*check earliest availability with same parameters as before. 
now with reservation #10 (days 7 to 10), no Queen rooms are available until one of the three stays completes.
the earliest completion is Reservation #4 (completes on day 8)
so output should be the day after which is 9 *)
earliestAvailable sys Queen 7 10;

(*make a new reservations to make it so that the availability of Queen rooms gets interrupted 
between day 7 to 16 (The span of the stay starting at day 7 for 10 nights)*)
 val sys = reserve sys ((11, "John","Gilmore",13,9,1,Queen):resrecord);
 val sys = reserve sys ((12, "Tim","Berners-Lee",15,12,1,Queen):resrecord);
 val sys = reserve sys ((13, "Larry","Wall",16,15,1,Queen):resrecord);
 
(*check earliest availability with same parameters as before.
now with reservation #11 (days 13 to 21)
reservation #12 (days 15 to 26)
reservation #13 (days 16 to 30)
a 10 day Queen room reservation no longer fits into days 7 to 16.
So the expected output should be the day after the earliest date either reservation #11, #12, #13 completes,
which is 22 *)
earliestAvailable sys Queen 7 10;