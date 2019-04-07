module BankAccount

type Account = {IsOpen: bool; mutable Balance: decimal}

let mkBankAccount() = {IsOpen = true; Balance = 0.0m}

let openAccount account = {account with IsOpen = true}

let closeAccount account = {account with IsOpen = false}

let getBalance account =
    if account.IsOpen then
        Some account.Balance
    else
        None

let updateBalance change account =
    if account.IsOpen then
        lock account (fun () -> account.Balance <- account.Balance + change)
        account
    else
        account