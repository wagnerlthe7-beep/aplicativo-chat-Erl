package com.chat.whaclone

import android.content.Intent
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import com.chat.whaclone.ui.theme.ClientandroidTheme

class ConversationsActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val username = intent.getStringExtra("username") ?: "Usuário"
        setContent {
            ClientandroidTheme {
                ConversationsScreen(username) { chatName ->
                    startActivity(Intent(this, ChatActivity::class.java).apply {
                        putExtra("chatName", chatName)
                    })
                }
            }
        }
    }
}

@Composable
fun ConversationsScreen(username: String, onChatClick: (String) -> Unit) {
    val chats = listOf("Alice", "Bob", "Charlie", "Grupo Família") // lista dummy

    Surface(modifier = Modifier.fillMaxSize(), color = MaterialTheme.colorScheme.background) {
        Column(modifier = Modifier.padding(16.dp)) {
            Text("Bem-vindo, $username!", style = MaterialTheme.typography.titleLarge)
            Spacer(modifier = Modifier.height(16.dp))
            LazyColumn {
                items(chats) { chat ->
                    Text(
                        text = chat,
                        style = MaterialTheme.typography.bodyLarge,
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(8.dp)
                            .clickable { onChatClick(chat) }
                    )
                    Divider()
                }
            }
        }
    }
}

