# Packages that use the `RecordWildCard` extension

## `chain` package
### File path prefix: src/Pos/Chain

| File | Types | Type Definition Location |
| --- | --- | ---|
|Update/Configuration.hs|UpdateConfiguration| Same module |
|Update/Poll/Failure.hs|PollVerFailure (PollTipMismatch)| Same module |
|Update/Poll/Types.hs|UndecidedProposalState| Same module |
|Update/BlockVersion.hs|BlockVersionData, BlockVersionModifier| core/src/Pos/Core/Update/BlockVersionModifier.hs |
|Update/Poll/Class.hs|SoftwareVersion | core/src/Pos/Core/Update/SoftwareVersion.hs |
|Update/Poll/Class.hs|UpdateProposal (UnsafeUpdateProposal) | core/src/Pos/Core/Update/Vote.hs |
|Block/Genesis/Instances.hs|GenericBlock b (UnsafeGenericBlock) | chain/src/Pos/Chain/Block/Blockchain.hs |
|Block/Genesis/Instances.hs|GenesisBody | chain/src/Pos/Chain/Block/Genesis/Types.hs |
|Block/Genesis/Types.hs|GenesisBody, GenesisConsensusData | Same module |
|Block/Main/Instances.hs|UnsafeGenericBlock | chain/src/Pos/Chain/Block/Blockchain.hs|
|Block/Main/Instances.hs|MainBody |chain/src/Pos/Chain/Block/Main/Types.hs|
|Block/JsonLog.hs|JLBlock |core/src/Pos/Core/JsonLog/LogEvents.hs|
|Block/Main/Types.hs|MainBody, MainExtraHeaderData, MainProof|Same module|
|Block/Logic/Integrity.hs|VerifyBlockParams, VerifyHeaderParams |Same module|
|Block/Types.hs|Undo|Same module|
|Block/BHelpers.hs|UnsafeGenericBlock |chain/src/Pos/Chain/Block/Blockchain.hs|
|Block/BHelpers.hs|MainBody |chain/src/Pos/Chain/Block/Main/Types.hs|
|Block/BHelpers.hs|GenericBlockHeader b (UnsafeGenericBlockHeader) |chain/src/Pos/Chain/Block/Blockchain.hs|
|Block/BHelpers.hs|MainConsensusData|chain/src/Pos/Chain/Block/Union/Types.hs|
|Block/BHelpers.hs|MainExtraHeaderData |chain/src/Pos/Chain/Block/Main/Types.hs|
|Delegation/Types.hs|DlgUndo|chain/src/Pos/Chain/Delegation/Types.hs|
|Block/Blockchain.hs|GenericBlockHeader b (UnsafeGenericBlockHeader), GenericBlock b (UnsafeGenericBlock) | Same module
|Block/Union/Types.hs|GenericBlockHeader b (UnsafeGenericBlockHeader) |chain/src/Pos/Chain/Block/Blockchain.hs|
|Block/Union/Types.hs|GenesisConsensusData |chain/src/Pos/Chain/Block/Genesis/Types.hs
|
|Block/Union/Types.hs|MainBody|chain/src/Pos/Chain/Block/Main/Types.hs|
|Block/Union/Types.hs|MainConsensusData |Same module|
|Txp/Toil/Stakes.hs|UnsafeTx |core/src/Pos/Core/Txp/Tx.hs|
|Txp/Base.hs|TxOutAux |core/src/Pos/Core/Txp/TxOutAux.hs|
|Txp/Base.hs|TxOut |core/src/Pos/Core/Txp/Tx.hs|
|Txp/Base.hs|UnsafeTxPayload |core/src/Pos/Core/Txp/TxPayload.hs|
|Txp/Toil/Logic.hs|VerifyTxUtxoRes |chain/src/Pos/Chain/Txp/Toil/Utxo/Functions.hs|
|Txp/Toil/Logic.hs|BlockVersionData |core/src/Pos/Core/Update/BlockVersionData.hs|
|Txp/Toil/Logic.hs|TxAux |core/src/Pos/Core/Txp/TxAux.hs|
|Txp/Toil/Logic.hs|AddrAttributes |core/src/Pos/Core/Common/AddrAttributes.hs|
|Security/Util.hs|SecurityParams|chain/src/Pos/Chain/Security/Params.hs|
|Txp/Toil/Failure.hs|TxOut |core/src/Pos/Core/Txp/Tx.hs|
|Ssc/Shares.hs|Commitment |core/src/Pos/Core/Ssc/Commitment.hs|
|Txp/Topsort.hs|TxAux |core/src/Pos/Core/Txp/TxAux.hs|
|Txp/Toil/Utxo/Functions.hs|VTxContext | Same module|
|Txp/Toil/Utxo/Functions.hs|TxOut, UnsafeTx |core/src/Pos/Core/Txp/Tx.hs|
|Txp/Toil/Utxo/Functions.hs|TxAux |core/src/Pos/Core/Txp/TxAux.hs|
|Ssc/Toss/Logic.hs|TossModifier |chain/src/Pos/Chain/Ssc/Toss/Types.hs|
|Ssc/Base.hs|Commitment |core/src/Pos/Core/Ssc/Commitment.hs|
|Ssc/Error/Verify.hs|SscVerifyError (TossInternalError)| Same module|
|Ssc/Seed.hs|Commitment|core/src/Pos/Core/Ssc/Commitment.hs|
|Ssc/Types.hs|SscGlobalState, SscParams |Same module|
|Ssc/VssCertData.hs|VssCertData |Same module|
|Ssc/Toss/Base.hs|Commitment|core/src/Pos/Core/Ssc/Commitment.hs|
|bench/block-bench.hs|TestSubject| Same module|
### File path prefix: test/Test/Pos/Chain

| File | Types | Type Definition Location |
| --- | --- | ---|
|Txp/CoreSpec.hs|UnsafeTx | core/src/Pos/Core/Txp/TxPayload.hs|
|Ssc/Arbitrary.hs|Commitment | core/src/Pos/Core/Ssc/Commitment.hs|
|Ssc/Arbitrary.hs|SlotId|core/src/Pos/Core/Slotting/SlotId.hs|
|Block/Arbitrary.hs|MainProof |src/Pos/Chain/Block/Main/Types.hs|
|Block/Arbitrary.hs|BodyDependsOnSlot | Same module|
|Block/Arbitrary.hs|SlotId|core/src/Pos/Core/Slotting/SlotId.hs|
|Txp/Toil/UtxoSpec.hs|UnsafeTx|core/src/Pos/Core/Txp/Tx.hs|
|Txp/Toil/UtxoSpec.hs|TxOutAux|core/src/Pos/Core/Txp/TxOutAux.hs|
|Txp/Toil/UtxoSpec.hs|ToilVerFailure (ToilWitnessDoesntMatch)|chain/src/Pos/Chain/Txp/Toil/Failure.hs|

