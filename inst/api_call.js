const fs = require('fs');

// Capture command-line arguments passed from R
const input = process.argv[2];
const model = process.argv[3];
const url = process.argv[4];
const api_key = process.argv[5];

// Log the inputs to verify they are correct
console.log('Input:', input);
console.log('Model:', model);
console.log('URL:', url);
console.log('API Key:', api_key);

// Continue with the rest of your logic, such as making the API request
async function getEmbedding() {
    try {
        const response = await fetch(url, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Authorization': `Bearer ${api_key}`
            },
            body: JSON.stringify({
                model: model,
                input: input
            })
        });

        if (!response.ok) {
            throw new Error(`HTTP error! Status: ${response.status}`);
        }

        const data = await response.json();
        console.log("API call successful, writing to data.json");

        // Write the received data to data.json
        fs.writeFileSync('inst/data.json', JSON.stringify(data));
    } catch (error) {
        console.error('Error during API call:', error.message);
        // Optionally, write the error to a log file
        fs.writeFileSync('inst/error.log', error.message);
    }
}

// Run the function
getEmbedding();