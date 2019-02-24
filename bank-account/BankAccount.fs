module BankAccount

type Account = {isOpen: bool; balance: decimal ref}

let mkBankAccount() = {isOpen = true; balance = ref 0.0m}

let openAccount account = {account with isOpen = true}

let closeAccount account = {account with isOpen = false}

let getBalance account =
    match account.isOpen with
    | false -> None
    | true -> Some !account.balance

let updateBalance change account =
    match account.isOpen with
    | false -> account
    | true -> let monitor = new obj()
              let newBalance = !account.balance + change
              lock monitor (fun () -> account.balance := newBalance)
              account