// Initialize SDHC memory card in SPI mode with Arduino

int CLK = 8; // bus clock pin
int CS = 13; // chip select pin
int DI = 12; // data input pin
int DO = 7;  // data output pin
int WAIT = 1; // clock frequency

void setup() {
	Serial.begin(9600);
	pinMode(CLK, OUTPUT);
	pinMode(CS, OUTPUT);
	pinMode(DI, OUTPUT);
	pinMode(DO, INPUT);

	byte CMD0[] {0x40, 0x00, 0x00, 0x00, 0x00, 0x95};    // Reset card
	byte CMD8[] {0x48, 0x00, 0x00, 0x01, 0xAA, 0x87};    // Initialize card
	byte CMD55[]{0x40+55, 0x00, 0x00, 0x00, 0x00, 0x01}; // Check ready state (part 1)
	byte CMD41[]{0x40+41, 0x40, 0x00, 0x00, 0x00, 0x01}; // part 2

	// Initialize SPI mode
	digitalWrite(CLK, LOW);
	digitalWrite(CS, HIGH);
	digitalWrite(DI, HIGH);
	dummyCycle(80);

	// Reset
	sendCmd(CMD0);
	recvR1();

	// Initialize card
	sendCmd(CMD8);
	recvR7();

	// Check ready state
	for (int i = 0; i<20; i++) {
		Serial.print("\nTRY ");
		Serial.println(i + 1);

		sendCmd(CMD55);
		recvR1();

		sendCmd(CMD41);
		if (recvR1()) {
			Serial.print("\nCard is ready");
			break;
		}
	}
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
	digitalWrite(CS, HIGH);
	digitalWrite(CS, LOW);
	digitalWrite(CLK, LOW);
	digitalWrite(CLK, HIGH);

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
	Serial.print(" ");
}

bool recvR1() {
	Serial.print("R1 ");

	// Wait for R1 response
	int i = 0;
	for (; i<=20; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);

		delay(WAIT);
		digitalWrite(CLK, HIGH);
		if (digitalRead(DO) == LOW) {
			Serial.print(i + 1);
			Serial.print(" wait cycles ");
			break;
		}
	}
	Serial.println("");
	if (i == 20) {
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
	delay(WAIT);
	digitalWrite(CLK, LOW);

	delay(WAIT);
	digitalWrite(CLK, HIGH);
	Serial.println("");

	return ready;
}

bool recvR7() {
	Serial.print("R7 ");
	
	// Wait for R7 response
	int i = 0;
	for (; i<=20; i++) {
		delay(WAIT);
		digitalWrite(CLK, LOW);
		delay(WAIT);
		digitalWrite(CLK, HIGH);
		if (digitalRead(DO) == LOW) {
			Serial.print(i + 1);
			Serial.print(" wait cycles ");
			break;
		}
	}
	Serial.println("");
	if (i == 20) {
		Serial.println("No Response");
		return false;
	}
	Serial.print("0"); // start bit
	bool ready = true;
	for (i = 0; i<47; i++) {
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
	delay(WAIT);
	digitalWrite(CLK, LOW);

	delay(WAIT);
	digitalWrite(CLK, HIGH);
	Serial.println("");

	return ready;
}
