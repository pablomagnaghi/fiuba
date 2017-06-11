package fiuba.algo3.tp1;

import java.util.ArrayList;
import java.util.List;

public class Chats {
	private List<Chat> chats;  
	
	public Chats() 
	{
		chats = new ArrayList<Chat>();
	}
	
	public int getCantidadDeContactos() 
	{
		return chats.size();
	}

	public void agregarChat(String nombre) {
		chats.add(new Chat(nombre));
		
	}
	
	public boolean existeContacto(String nombreContacto) {
		if (getContacto(nombreContacto) != null) {
			return true;
		}
		return false;
	}

	public int cantidadTotalMensajesRecibidos() {
		int cantidadTotalMensajesRecibidos = 0;
		
		for (Chat contacto : chats) {
			cantidadTotalMensajesRecibidos += contacto.cantidadTotalMensajesRecibidos();
		}
		
		return cantidadTotalMensajesRecibidos;
	}

	public int cantidadMensajesDe(String nombreContacto) {		
		
		for (Chat contacto : chats) {
			if (contacto.getNombre().equals(nombreContacto)) {
				return contacto.cantidadDeMensajes();
			}
		}
			
		return 0;
	}

	public void recibirMensajeDe(String nombreContacto, String mensaje) {
		Chat contacto = getContacto(nombreContacto);
		
		if (contacto != null) {
			contacto.agregarMensajeRecibido(mensaje);
		}
		else {
			Chat nuevoContacto = new Chat(nombreContacto);
			nuevoContacto.agregarMensajeRecibido(mensaje);
			chats.add(nuevoContacto);
		}	
	}
	
	public void enviarMensajeA(String nombreContacto, String mensaje) {
		Chat contacto = getContacto(nombreContacto);
		
		if (contacto != null) {
			contacto.agregarMensajeEnviado(mensaje);
		}
		else {
			Chat nuevoContacto = new Chat(nombreContacto);
			nuevoContacto.agregarMensajeEnviado(mensaje);
			chats.add(nuevoContacto);
		}
	}

	private Chat getContacto(String nombreContacto) {
		for (Chat contacto : chats) {
			if (contacto.getNombre().equals(nombreContacto)) {
				return contacto;
			}
		}
		return null;
	}

	public int cantidadDeMensajesEnviados() {
		int cantidadTotalMensajesEnviados = 0;
		
		for (Chat contacto : chats) {
			cantidadTotalMensajesEnviados += contacto.cantidadTotalMensajesEnviados();
		}
		
		return cantidadTotalMensajesEnviados;	
	}

	public int cantidadMensajesEnviadosA(String nombreContacto) {
		for (Chat contacto : chats) {
			if (contacto.getNombre().equals(nombreContacto)) {
				return contacto.cantidadTotalMensajesRecibidos();
			}
		}
			
		return 0;	
	}

	public List<String> obtenerConversacionCon(String nombreContacto) {
		
		Chat contacto = getContacto(nombreContacto);
		
		if (contacto != null) {
			return contacto.getConversacion(nombreContacto);
		}
		return null;
	}
}
