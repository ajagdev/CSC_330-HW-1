(*  Abhi Jagdev - V00746164 *)
(*  Assignment #1 *)

type DATE = {year:int, month:int, day: int}
exception InvalidParameter

(* This file is where your solutions go *)
		     
fun is_older(d1: DATE, d2: DATE): bool =
    if #year d1 = #year d2 andalso #month d1 = #month d2 andalso #day d1 = #day d2
    then false
    else
	if #year d1 < #year d2
	then false
	else
	    if #year d1 = #year d2 andalso #month d1 < #month d2
	    then false
	    else
		if #year d1 = #year d2 andalso #month d1 = #month d2 andalso #day d1 < #day d2
		then false
		else true

(* Add your other functions here *)
fun number_in_month( dates: DATE list , month_given: int) =
    if #month (hd dates) = month_given
    then
	if null (tl dates)
	then 1
	else 1 + number_in_month( (tl dates), month_given )
    else
	if null (tl dates)
	then 0
	else 0 + number_in_month( (tl dates), month_given )

fun number_in_months( dates: DATE list, months_given: int list) =
    if null (tl months_given)
    then number_in_month(dates, (hd months_given))
    else number_in_month(dates, (hd months_given)) + number_in_months( dates, (tl months_given) )

fun dates_in_month(dates: DATE list, month_given: int) =
    if #month (hd dates) = month_given
    then
	if null (tl dates)
	then (hd dates) :: []
	else (hd dates) :: dates_in_month( (tl dates), month_given )
    else
	if null (tl dates)
	then []
	else dates_in_month( (tl dates), month_given )

fun dates_in_months(dates: DATE list, months_given: int list): DATE list =
    if null (tl months_given)
    then dates_in_month(dates, (hd months_given))
    else dates_in_month(dates, (hd months_given)) @ dates_in_months( dates, (tl months_given) )

fun get_nth(str: string list,n: int) =
    if n = 0 orelse n > (length str)
    then raise InvalidParameter
    else
	if n = 1
	then hd str
	else get_nth( (tl str), n-1)

fun date_to_string(d:DATE): string =
    let
	val m = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth( m, (#month d)) ^ " " ^ Int.toString(#day d) ^ ", " ^ Int.toString(#year d)
    end

fun number_before_reaching_sum(sum: int, xs: int list): int =
    if (hd xs) >= sum
    then 0
    else 1 + number_before_reaching_sum ( (sum-hd xs), tl xs); 
		
fun what_month(x: int): int =
    if x = 0
    then 0
    else
	let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in 1 + number_before_reaching_sum ( x, days_in_months)
	end

fun month_range(day1: int, day2: int): int list =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

fun oldest(d: DATE list) =
    if null d
    then NONE
    else
	let
	    fun check_dates( xs: DATE list ) =
		if null (tl xs)
		then (hd xs)
		else
		    let
			val tl_old = check_dates(tl xs)
		    in
			if is_older((hd xs), tl_old)
			then hd xs
			else tl_old
		    end
	in SOME	(check_dates(d))
	end
	
fun reasonable_date(d: DATE): bool =
    if #year d < 1 orelse #month d > 12 orelse #month d < 1 orelse #day d < 1
    then false
    else
	let
	    fun leap_year( year: int): bool =
		if (year mod 4) <> 0
		then false
		else
		    if (year mod 100) <> 0
		    then true
		    else
			if (year mod 400) <> 0
			then false
			else true
	in
	    let
		fun check_day(day: int, days_in_month: string list, month: int): bool =
		    if day > valOf (Int.fromString (get_nth(days_in_month, month)))
		    then false
		    else true
	    in
		if leap_year(#year d)
		then
		    let
			val m = ["31", "29", "31", "30", "31", "30", "31", "31", "30", "31", "30", "31"]
		    in
			if check_day(#day d, m, #month d)
			then true
			else false 
		    end
		else
		    let
			val m = ["31", "28", "31", "30", "31", "30", "31", "31", "30", "31", "30", "31"]
		    in
			if check_day(#day d, m, #month d)
			then true
			else false 
		    end
	    end
	end
