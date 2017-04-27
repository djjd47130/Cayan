# Genius Integration

The Cayan Component Library for Delphi allows you to integrate with the Genius solutions. This includes communication with the different types of devices which Cayan provides for payment collection.

## `TCayanGenius` Component

This component provides direct access to Cayan's Genius systems, and a single CED payment terminal. To support multiple terminals, you can use different instances of `TCayanGenius`, each connected to the same `TCayan` component for authorization. 

You can perform many different things using this component. This includes the following:

1. Request payment from customer, and wait indefinitely inside of a worker thread. An event `OnTransactionResult` will be triggered when a response is returned from the device.
2. Utilize Line Item Display, showing the customer each item they are purchasing, along with a total. 
3. Cancel various requests which may be in progress.
4. Capture customer signature in vector format and convert to bitmap.
5. Initiate keyed sale, overriding the device in favor for manual payment entry. 
6. Perform transactions using vault tokens in absense of customer.


