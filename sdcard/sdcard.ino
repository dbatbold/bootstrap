// Initialize SDHC memory card in SPI mode with Arduino

int CLK = 8; // bus clock pin
int CS = 13; // chip select pin
int DI = 12; // data input pin
int DO = 7;  // data output pin
int WAIT = 1; // clock frequency

void setup() {
	delay(1000);
	Serial.begin(9600);
	pinMode(CLK, OUTPUT);
	pinMode(CS, OUTPUT);
	pinMode(DI, OUTPUT);
	pinMode(DO, INPUT);

	byte CMD0[] {0x40, 0x00, 0x00, 0x00, 0x00, 0x95};    // Reset card
	byte CMD8[] {0x48, 0x00, 0x00, 0x01, 0xAA, 0x87};    // Initialize card
	byte CMD55[]{0x40+55, 0x00, 0x00, 0x00, 0x00, 0x01}; // Check ready state
	byte CMD41[]{0x40+41, 0x40, 0x00, 0x00, 0x00, 0x01}; // CMD55 + CMD41 = ACMD41
	byte CMD16[]{0x40+16, 0x00, 0x00, 0x00, 0x08, 0x01}; // Set data block size (try 8 bytes)
	byte CMD24[]{0x40+24, 0x00, 0x00, 0x00, 0x00, 0x01}; // Write single block to address zero

	// Initialize SPI mode
	digitalWrite(CLK, LOW);
	digitalWrite(CS, HIGH);
	digitalWrite(DI, HIGH);
	dummyCycle(74);

	// Reset
	sendCmd(CMD0);
	recvR1();
	Serial.println("");

	// Initialize card
	sendCmd(CMD8);
	recvR7();

	// Check ready state
	int i = 0;
	for (; i<20; i++) {
		Serial.print("\nTRY ");
		Serial.println(i + 1);

		sendCmd(CMD55);
		recvR1();

		sendCmd(CMD41);
		if (recvR1()) {
			Serial.println("\nCard is ready\n");
			break;
		}
	}
	if (i == 20) {
		Serial.print("\nCard failed to initialize");
		return;
	}

	// Set block size for data transfter
	sendCmd(CMD16);
	if (!recvR1()) {
		Serial.println("CMD16 failed");
		return;
	}

	// Write 8 bytes
	sendCmd(CMD24);
	if (!recvR1()) {
		Serial.println("CMD24 failed");
		return;
	}
	sendDataPacket();
	dataResponse();
	waitWriteResponse();

	digitalWrite(CLK, LOW);
	digitalWrite(DO, LOW);
	digitalWrite(CS, HIGH);
}

void loop() {}

void dummyCycle(int n) {
	for (int i = 0; i<n; i++) {
		delay(WAIT);
		digitalWrite(CS, HIGH);
		digitalWrite(CLK, LOW);

		delay(WAIT);
		digitalWrite(CS, LOW);
		digitalWrite(CLK, HIGH);
	}
}

void sendCmd(byte cmd[]) {
	// Start command frame
	delay(WAIT);
	digitalWrite(CLK, LOW);
	delay(WAIT);
	digitalWrite(CS, HIGH);
	delay(WAIT);
	digitalWrite(CS, LOW);

	// SD card command number
	int cmdNum = cmd[0] - 0x40;
	Serial.print("CMD");
	Serial.println(cmdNum);

	// Send command
	for (int i = 0; i<6; i++) {
		sendByte(cmd[i]);
	}

	Serial.println("");
}

void sendByte(byte b) {
	for (int i = 0; i<8; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);
		if ((b & 0x80) == 0x80) {
			digitalWrite(DI, HIGH);
			Serial.print("1");
		} else {
			digitalWrite(DI, LOW);
			Serial.print("0");
		}
		delay(WAIT);
		digitalWrite(CLK, HIGH);
		b = b << 1;
	}
	delay(WAIT);
	digitalWrite(CLK, LOW);
	digitalWrite(DI, LOW);
	delay(WAIT);
	Serial.print(" ");
}

bool recvR1() {
	Serial.print("R1 ");
	digitalWrite(CLK, LOW);
	delay(WAIT);
	delay(WAIT);

	// Wait for R1 response
	int i = 0;
	for (; i<40; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);

		delay(WAIT);
		digitalWrite(CLK, HIGH);
		if (digitalRead(DO) == LOW) {
			Serial.print(i);
			Serial.print(" wait cycles ");
			break;
		}
	}
	Serial.println("");
	if (i == 40) {
		Serial.println("No Response");
		return false;
	}
	Serial.print("0"); // start bit
	bool ready = true;
	for (i = 0; i<7; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);

		delay(WAIT);
		digitalWrite(CLK, HIGH);
		int out = digitalRead(DO);
		if (out == HIGH) {
			ready = false;
		}
		Serial.print(out);
	}
	Serial.println("");

	delay(WAIT);
	digitalWrite(CLK, LOW);
	delay(WAIT);

	return ready;
}

bool recvR7() {
	Serial.print("R7 ");
	
	// Wait for R7 response
	int i = 0;
	for (; i<40; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);

		delay(WAIT);
		digitalWrite(CLK, HIGH);
		if (digitalRead(DO) == LOW) {
			Serial.print(i);
			Serial.print(" wait cycles ");
			break;
		}
	}
	Serial.println("");
	if (i == 40) {
		Serial.println("No Response");
		return false;
	}
	Serial.print("0"); // start bit
	for (i = 0; i<39; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);

		delay(WAIT);
		digitalWrite(CLK, HIGH);
		int out = digitalRead(DO);
		Serial.print(out);
	}
	Serial.println("");
}

bool sendDataPacket() {
	// Test write command
	byte DataToken = 0xFE;
	byte DataBlock[]{0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x8}; // Data block
	byte DataCRC[]{0x00, 0x00}; // Data checksum (disabled in SPI mode)

	Serial.println("\nData token");

	sendByte(0x00); // 1 byte cycle time
	sendByte(DataToken);

	Serial.println("\nData block");
	for (int i = 0; i<8; i++) {
		sendByte(DataBlock[i]);
	}
	Serial.println("\nData CRC");
	for (int i = 0; i<2; i++) {
		sendByte(DataCRC[i]);
	}
}

bool dataResponse() {
	Serial.print("\nData response\n");
	delay(WAIT);
	delay(WAIT);
	for (int i=0; i<8; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);

		delay(WAIT);
		digitalWrite(CLK, HIGH);
		int out = digitalRead(DO);
		Serial.print(out);
	}
	Serial.println("");
	delay(WAIT);
	delay(WAIT);
}

bool waitWriteResponse() {
	Serial.print("\nWaiting for write command response ");
	int busy = 1024*20;

	delay(WAIT);
	delay(WAIT);

	// Wait for data response
	int i = 0;
	for (; i<busy; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);

		delay(WAIT);
		digitalWrite(CLK, HIGH);
		if (digitalRead(DO) == HIGH) {
			Serial.print(i);
			Serial.print(" wait cycles ");
			break;
		}
	}
	Serial.print("\nbusy cycles ");
	Serial.println(i);
	if (i == busy) {
		Serial.println("No Response");
		return false;
	}
	Serial.print("1"); // start bit
	for (i = 0; i<7; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);

		delay(WAIT);
		digitalWrite(CLK, HIGH);
		int out = digitalRead(DO);
		Serial.print(out);
	}
	Serial.println("");
}

