# Chapter 4 - Genius Transactions

The core base of credit card transactions on the Genius platform are done through the `TCayanGeniusTransaction` component.

## `TCayanGeniusTransaction` Component

A single possible transaction is encapsulated within an instance of a `TCayanGeniusTransaction` component. It provides direct access to performing card transactions via Genius solutions. It also allows you to add on a `TCayanGeniusLineItems` component to implement Line Item Display. It is event driven, so your application should have some means of waiting for a response indefinitely. 

NOTE: Be sure to assign the `Genius` property with an appropriate corresponding `TCayanGenius` component to point it to a specific CED payment terminal. 

### Preparing Transactions

To start a new transaction, fill out the properties available on this component. Pay attention to the `TransactionType` property, as this identifies whether it is a sale, refund, etc. By default, for basic transactions, leave this at its default value `gtSale`. 

- `TransactionType` - Identifies the specific type of transaction to be performed.
- `Amount` - The total dollar amount of the transaction. This includes tax.
- `TaxAmount` - The total tax amount of the transaction. 
- `InvoiceNum` - The merchant defined order number / invoice number transaction is associated with.
- `TransactionID` - The merchant defined identifier of the transaction.
- `PONumber` - The Purchase Order Number the transaction is associated with.
- `CustomerCode` - The unique identifier of the customer.
- `Cardholder` - The name on the card which will be used.

### Performing Transactions

Once you have prepared the transaction, you are ready to send the command to the device to capture card information and perform the actual transaction. This is done using the procedure `TCayanGeniusTransaction.StartTransaction`. Please note that if there is any chance at this point that the customer may choose a different form of payment (such as cash or check), then you should not use this. Instead, if you are using Line Item Display, then you need to use `TCayanGeniusLineItems.EndOrder` to let the device know a different payment method has been used. 

From the moment you call `StartTransaction`, you should wait indefinitely. Please refer to the section below about [Waiting For Response](./Chapter%204%20-%20Genius%20Transactions.md#waiting-for-response). 

### Events

The following events are available on this component:

1. `OnTransactionStaged` (Optional) - Occurs when a new transaction has been staged. This is the first step to collecting payments from a Genius CED payment terminal. This property is not required, but you may wish to use it to capture information.
2. `OnTransactionStart` (Optional) - Occurs when the command is sent to the Genius CED payment terminal to capture payment information. If you are performing transactions through the CED, then you can use this event to detect when you should start waiting for the device. 
3. `OnTransactionResult` (Required) - Occurs when the Genius CED payment terminal is completed with capturing payment information. This does not necessarily mean that a payment has been successful. You must read the transaction response in this event to determine the result of the transaction.
4. `OnException` (Required) - Occurs when an exception of any kind was raised when starting the transaction. This should be handled to abort the waiting procedure of your application. This only occurs after the moment you call `StartTransaction`. If `OnException` is called, then `OnTransactionResult` will never be called. It's always going to be one or the other. 

### Waiting For Response

Between the events `OnTransactionStart` and `OnTransactionResult`, your application must wait indefinitely. This could mean for hours, technically. You shall not allow the user to do anything other than the following actions:

1. `Cancel` - Send a command to the device to cancel the current transaction.
2. `Initiate Keyed Entry` - Send a command to the device to collect payment information via manual keyed card number.

