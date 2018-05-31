type t = Enemy.t
type count = Enemy.count
type enemies = Enemy.party list
type report = (count * t) list
type sum_report = (count * t list)

val cost : Resource.t

val report_of : enemies -> report
val sum_report_of : enemies -> sum_report
