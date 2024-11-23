import {
    Clarinet,
    Tx,
    Chain,
    Account,
    types
} from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "User can register profile",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const wallet1 = accounts.get('wallet_1')!;
        
        let block = chain.mineBlock([
            Tx.contractCall('time-exchange', 'register-user', [
                types.utf8("John Doe"),
                types.utf8("Programming, Design")
            ], wallet1.address)
        ]);
        
        block.receipts[0].result.expectOk();
    },
});

Clarinet.test({
    name: "User can create and accept service requests",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        const wallet1 = accounts.get('wallet_1')!;
        const wallet2 = accounts.get('wallet_2')!;
        
        let block = chain.mineBlock([
            Tx.contractCall('time-exchange', 'mint-initial-credits', [
                types.principal(wallet1.address)
            ], wallet1.address),
            Tx.contractCall('time-exchange', 'create-request', [
                types.utf8("Website Development"),
                types.utf8("Need help building a website"),
                types.uint(5)
            ], wallet1.address),
            Tx.contractCall('time-exchange', 'accept-request', [
                types.uint(0)
            ], wallet2.address)
        ]);
        
        block.receipts.forEach(receipt => {
            receipt.result.expectOk();
        });
    },
});
