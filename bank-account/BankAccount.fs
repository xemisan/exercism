module BankAccount

type Account = { mutable Balance: decimal option }

let mkBankAccount() = { Balance = None }

let openAccount account = { account with  Balance = Some 0.0m }

let closeAccount account = { account with Balance = None }

let getBalance account = account.Balance

let updateBalance change account =
    lock account (fun () -> Option.iter (fun x -> account.Balance <- Some (x + change)) account.Balance)
    account