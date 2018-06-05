const cobinhood = require('node-cobinhood-api');
 
cobinhood.options({
            'apiKey': 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhcGlfdG9rZW5faWQiOiIzOWYzYTJmOS0wNDBkLTRhMzgtYjZiMi01ZWUyYmFjNTVlNTkiLCJzY29wZSI6WyJzY29wZV9leGNoYW5nZV90cmFkZV9yZWFkIiwic2NvcGVfZXhjaGFuZ2VfdHJhZGVfd3JpdGUiLCJzY29wZV9leGNoYW5nZV9sZWRnZXJfcmVhZCJdLCJ1c2VyX2lkIjoiYzMwNjBiNDctM2YyZS00MGNiLWIyMDItZmI4YTI2NWNjNmE1In0.R73FtdF4kSd2IdXRFK7E02XPKMwAxc4hsGrJp7ooR1E.V2:ebb11f7fefc9e39be7ed532d738d5f528d8c3b7dee6cd41ad8a60ce6be3e800a',
            'verbose': true
});

cobinhood.lastPrice("COB-BTC", (error, lastPrice) => {
    if (!error) {
        console.log("COB-BTC last price:", lastPrice);
        // COB-BTC last price: 0.00001687
    }
});


let price = 0.000017;
let quantity = 1000;
 
cobinhood.checkOrder("COB-BTC", price, quantity, (error, order) => {
        if (!error) {
            console.log(order);
        }
        console.log(error);
});
