# Chapter 3 - Genius Integration

The Cayan Component Library for Delphi allows you to integrate with the Genius solutions. This includes communication with the different types of devices which Cayan provides for payment collection.

## `TCayanGenius` Component

This component provides direct access to Cayan's Genius systems, and a single CED payment terminal. To support multiple terminals, you can use different instances of `TCayanGenius`, each connected to the same `TCayan` component for authorization. 

You can perform many different things using this component. This includes the following:

1. Request payment from customer, and wait indefinitely inside of a worker thread. An event `OnTransactionResult` will be triggered when a response is returned from the device.
2. Cancel various requests which may be in progress.
3. Initiate keyed sale, overriding the device in favor for manual payment entry. 
4. Perform transactions using vault tokens in absense of customer.

Additionally, there are numerous other components which you can attach a `TCayanGenius` component to:

1. `TCayanGeniusLineItems` - Utilize line item display on the CED payment terminal.
2. `TCayanGeniusAgreement` - Prompt customer to accept or decline an agreement text on the CED payment terminal.
3. `TCayanGeniusSignature` - Prompt customer to sign the CED payment terminal and acquire vector format signature data.
4. `TCayanGeniusInput` - Prompt customer to enter either text, numeric, or date values on the CED payment terminal.
5. 



[Chapter 4 - Genius Transactions](./Readme/Chapter%204%20-%20Genius%20Transactions.md)
