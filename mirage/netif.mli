include Mirage_net.S

val connect
  :  ?umem_config:Xsk.Umem.Config.t
  -> ?socket_config:Xsk.Socket.Config.t
  -> string
  -> int
  -> t Lwt.t
