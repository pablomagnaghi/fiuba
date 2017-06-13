package fiuba.algo3.tp1;

import java.util.List;

public abstract class Chats {
	
	public abstract int cantidadDeChats();
	public abstract int cantidadTotalMensajesRecibidos();
	public abstract int cantidadTotalMensajesEnviados();
	public abstract boolean existeChat(String nombreChat);
	public abstract List<String> obtenerConversacionCon(String nombreChat);
	public abstract void enviarMensajeA(String nombreChat, String mensaje);
	public abstract void borrarMensajes(String nombreChat);
}
