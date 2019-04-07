module BankAccount

type Account = { mutable Balance: decimal option }

let mkBankAccount() = { Balance = Some 0.0m }

let openAccount _ = { Balance = Some 0.0m }

let closeAccount account = { account with Balance = None }

let getBalance account = account.Balance

let updateBalance change account =
    match account.Balance with
    | Some _ ->
        lock account (fun () -> account.Balance <- Some (account.Balance.Value + change))
        account
    | None ->
        account