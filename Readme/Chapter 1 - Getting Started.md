# Chapter 1 - Getting Started

Before you begin, you will need to become a developer partner with Cayan. They will create an account for you and provide you your own credentials and certification packet to get started. Once you have acquired this, you can continue using this library.

## `TCayan` Component

The first step of using this library in your application is to drop a `TCayan` component from the installed components on the `Cayan` tab in Delphi. This component is the starting point for the rest of the integration.

You will need to populate some of the properties of this component.

1. `MerchantName` (Required) - One of the 3 pieces of credentials provided by Cayan.
2. `MerchantSiteId` (Required) - One of the 3 pieces of credentials provided by Cayan.
3. `MerchantKey` (Required) - One of the 3 pieces of credentials provided by Cayan.
4. `Dba` (Required for Genius) - The name of the company as it will be displayed to the customer.
5. `SoftwareName` (Required for Genius) - The name of the software using the Genius solutions.
6. `SoftwareVersion` (Required for Genius) - The version of the software using the Genius solutions.
7. `ClerkID` (Required) - The username or identification who is using the software.
8. `StationID` (Required) - The identifier for the station within the store / location / site.
9. `TestMode` (Optional) - Triggers the library to run in test mode with fake credentials and transactions.

## `TCayanGenius` Component

If you wish to use Cayan's Genius solution, then drop a `TCayanGenius` component from the installed components on the `Cayan` tab in Delphi. This component is used for all communication with a specific Genius CED payment terminal. Multiple terminals will require multiple instances of this component.

You will need to populate some of the properties of this component.

1. `Cayan` (Required) - A previously created `TCayan` component for authentication.
2. `Device.DeviceAddress` (Required) - The Hostname or IP Address of the Genius CED.
3. `Device.DevicePort` (Required) - The Port Number of the Genius CED (8080 by default).
4. `Device.DeviceProtocol` (Required) - The HTTP protocol which the device requires.
5. `Device.DeviceTimeout` (Required) - The number of seconds before requests are timed out. `DEPRECATED`
6. `Device.DeviceVersion` (Required) - The highest support level of the device.
7. `Device.Monitoring` (Optional) - Whether or not to continuously check the status of the device.
8. `ForceDuplicate` (Optional) - Whether or not to force duplicate transactions, if detected.
9. `InvoiceNum` (Required) - The invoice number associated with transaction.
10. `PONumber` (Required for Level2) - The purchase order number associated with transaction.
11. `Amount` (Required) - The total dollar amount which transaction is to be for (including tax).
12. `TaxAmount` (Required for Level2) - The dollar amount of tax which transaction is to be for.
13. `Cardholder` (Optional) - Predefined card holder's name.
14. `CustomerCode` (Required for Level2) - The unique identifier of customer's account.
15. `TransactionID` (Required) - The unique identifier of the transaction being done.
16. `TransactionType` (Required) - The type of transaction which is being done.

Additionally, there are some events on this component:

1. `OnCancel` (Required) - Triggered when the POS has sent a cancel command to the device.
2. `OnDeviceStatus` (Optional) - Triggered when the `Device.Monitoring` property is enabled.
3. `OnTransactionStaged` (Optional) - Triggered when a new transaction has been staged.
4. `OnTransactionStart` (Optional) - Triggered when a transaction has begun on the device.
5. `OnTransactionResult` (Required) - Triggered when the device has finished a transaction.

## `TCayanGeniusAgreement` Component

If you wish to be able to prompt a customer to accept or decline an agreement on the device, then you will need to drop a `TCayanGeniusAgreement` component in your application. This component allows you to send the request and get a response.

You will need to populate some of the properties of this component.

1. `Genius` (Required) - A previously created `TCayanGenius` component for authentication.
2. `Title` (Optional) - The title at the top of the device over the agreement.
3. `AgreementText` (Required) - The actual text which is displayed to the customer.
4. `AcceptLabel` (Optional) - The label of the `Accept` button on the device.
5. `DeclineLabel` (Optional) - The label of the `Decline` button on the device.

Additionally, there are some events on this component:

1. `OnAgreement` (Required) - Triggered when the customer presses `Accept`, `Decline`, or when the device times out.


[Chapter 2 - MerchantWare Transactions](./Readme/Chapter%202%20-%20MerchantWare%20Transactions.md)
