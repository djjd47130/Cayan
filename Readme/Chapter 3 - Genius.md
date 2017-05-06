# Chapter 3 - Genius Integration

The Cayan Component Library for Delphi allows you to integrate with the Genius solutions. This includes communication with the different types of devices which Cayan provides for payment collection.

## `TCayanGenius` Component

This component provides direct access to Cayan's Genius systems, and a single CED payment terminal. To support multiple terminals, you can use different instances of `TCayanGenius`, each connected to the same `TCayan` component for authorization. 

This component is to define a device. It is not meant to actually perform transactions alone. Here are all the possible components which link up with this one:

1. `TCayanGeniusTransaction` - Implement a single payment transaction with a Genius device.
2. `TCayanGeniusLineItems` - Utilize line item display on the CED payment terminal.
3. `TCayanGeniusAgreement` - Prompt customer to accept or decline an agreement text on the CED payment terminal.
4. `TCayanGeniusSignature` - Prompt customer to sign the CED payment terminal and acquire vector format signature data.
5. `TCayanGeniusInput` - Prompt customer to enter either text, numeric, or date values on the CED payment terminal.

[Chapter 4 - Genius Transactions](./Chapter%204%20-%20Genius%20Transactions.md)
