import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Blob "mo:base/Blob";
import Bool "mo:base/Bool";
import Error "mo:base/Error";
import Debug "mo:base/Debug";

shared(msg) actor class burn(ledger_cid: Principal, governance_cid: Principal) = this {
    type Metadata = {
        ledger_cid: Principal;
        governance_cid: Principal;
    };
    type Account = {
        owner : Principal;
        subaccount : ?Blob;
    };
    type TransferArg = {
        from_subaccount : ?Blob;
        to : Account;
        amount : Nat;
        fee : ?Nat;
        memo :?Blob;
        created_at_time: ?Nat64;
    };
    type TransferError = {
        #BadFee : { expected_fee: Nat};
        #BadBurn : { min_burn_amount : Nat };
        #InsufficientFunds : { balance : Nat };
        #TooOld;
        #CreatedInFuture : { ledger_time : Nat64 };
        #TemporarilyUnavailable;
        #Duplicate : { duplicate_of : Nat };
        #GenericError : { error_code : Nat; message : Text };
    };

    type TransferResult = {
        #Ok : Nat;
        #Err : TransferError;
    };
    type Ledger = actor {
        icrc1_minting_account : query () -> async ?Account;
        icrc1_balance_of : query (Account) -> async Nat;
        icrc1_transfer : (TransferArg) -> async TransferResult;
    };
    type Result = { 
        #Ok : Text;
        #Err : Text 
    };
    private let ledger: Ledger = actor(Principal.toText(ledger_cid));
    private var lock: Bool = false;
    public query func metadata() : async Metadata {
        return {
            ledger_cid = ledger_cid;
            governance_cid = governance_cid;
        };
    };
    public shared({caller}) func burn() : async Result {
        Debug.print("==>burn.caller=" # Principal.toText(caller));
        if (not Principal.equal(caller, governance_cid)) {
            return #Err("permission_denied");
        };
        let cid: Principal = Principal.fromActor(this);
        if (lock) {
            return #Err("locked");
        };
        lock := true;
        try {
            var result: Result = switch(await ledger.icrc1_minting_account()) {
                case (?acc) {
                    let balance = await ledger.icrc1_balance_of({ owner = cid; subaccount = null; });
                    if (balance > 0) {
                        switch (await ledger.icrc1_transfer({
                            from_subaccount = null;
                            to = { owner = acc.owner; subaccount = acc.subaccount; };
                            amount = balance;
                            fee = null;
                            memo = null;
                            created_at_time = null;
                        })) {
                            case (#Ok n) {
                                #Ok(Nat.toText(n))
                            };
                            case (#Err e) {
                                #Err(debug_show(e))
                            };
                        };
                    } else {
                        #Err("no_balance")
                    };
                };
                case (_) {#Err("no_minting_account")};
            };
            lock := false;
            return result;
        } catch (err) {
            lock := false;
            return #Err(Error.message(err));
        };
    };
    public shared({caller}) func validate_burn() : async Result {
        Debug.print("==>burn.validate_burn=" # Principal.toText(caller));
        if (not Principal.equal(caller, governance_cid)) {
            return #Err("permission_denied");
        };
        return #Ok("ok");
    };
    system func inspect({
        arg : Blob;
        caller : Principal;
        msg : {
            #burn : () -> (); 
            #metadata : () -> (); 
            #validate_burn : () -> ()
        };
    }) : Bool {
        switch (msg) {
            case (#burn _) {
                return Principal.equal(caller, governance_cid);
            };
            case (#validate_burn _) {
                return Principal.equal(caller, governance_cid);
            };
            case (#metadata _) {
                return true;
            };
        };
    };
} 