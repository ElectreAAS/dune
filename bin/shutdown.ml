open Import
module Client = Dune_rpc_client.Client

let info =
  let doc = "Cancel and shutdown any builds in the current workspace." in
  Cmd.info "shutdown" ~doc
;;

let term =
  let+ builder = Common.Builder.term in
  Rpc.Rpc_common.client_term
    builder
    (Rpc.Rpc_common.fire_message
       ~name:"shutdown_cmd"
       ~wait:false
       builder
       (Rpc.Rpc_common.Notification Dune_rpc_private.Public.Notification.shutdown))
;;

let command = Cmd.v info term
