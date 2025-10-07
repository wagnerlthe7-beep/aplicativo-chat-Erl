package com.chat.whaclone

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import com.chat.whaclone.ui.theme.ClientandroidTheme

class ChatActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val chatName = intent.getStringExtra("chatName") ?: "Chat"
        setContent {
            ClientandroidTheme {
                ChatScreen(chatName)
            }
        }
    }
}

@Composable
fun ChatScreen(chatName: String) {
    var message by remember { mutableStateOf("") }
    val messages = remember { mutableStateListOf<String>() }

    Surface(modifier = Modifier.fillMaxSize(), color = MaterialTheme.colorScheme.background) {
        Column(modifier = Modifier.padding(16.dp)) {
            Text(chatName, style = MaterialTheme.typography.titleLarge)
            Spacer(modifier = Modifier.height(16.dp))
            LazyColumn(modifier = Modifier.weight(1f)) {
                items(messages) { msg ->
                    Text(msg, modifier = Modifier.padding(4.dp))
                }
            }
            Row(modifier = Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                OutlinedTextField(
                    value = message,
                    onValueChange = { message = it },
                    modifier = Modifier.weight(1f),
                    placeholder = { Text("Digite uma mensagem") }
                )
                Button(onClick = {
                    if(message.isNotEmpty()) {
                        messages.add("Você: $message")
                        message = ""
                    }
                }) {
                    Text("Enviar")
                }
            }
        }
    }
}
