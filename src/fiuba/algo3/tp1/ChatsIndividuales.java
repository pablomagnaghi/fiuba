package fiuba.algo3.tp1;

import java.util.ArrayList;
import java.util.List;

public class ChatsIndividuales extends Chats {

	private List<Contacto> contactos;
	

	private Contacto getContacto(String nombre) {
		for (Contacto contacto : contactos) {
			if (contacto.getNombre().equals(nombre)) {
				return contacto;
			}
		}
		return null;
	}
	
	public ChatsIndividuales() {
		contactos = new ArrayList<Contacto>();
	}

	public int cantidadDeContactos() {
		return contactos.size();
	}

	public void agregarChatConContacto(String nombre) {
		contactos.add(new Contacto(nombre));
	}
	
	public void recibirMensajeDe(String nombre, String mensaje) {
		getContacto(nombre).recibirMensaje(mensaje);
	}

	public int cantidadMensajesRecibidosDe(String nombre) {
		return getContacto(nombre).cantidadTotalMensajesRecibidos();
	}

	public int cantidadMensajesEnviadosA(String nombre) {
		return getContacto(nombre).cantidadTotalMensajesEnviados();
	}

	@Override
	public int cantidadDeChats() {
		return contactos.size();
	}

	@Override
	public int cantidadTotalMensajesRecibidos() {
		int mensajesRecibidos = 0;
		for (Contacto contacto : contactos) {
			mensajesRecibidos += contacto.cantidadTotalMensajesRecibidos();
		}
		return mensajesRecibidos;
	}

	@Override
	public int cantidadTotalMensajesEnviados() {
		int mensajesEnviados = 0;
		for (Contacto chatConContacto : contactos) {
			mensajesEnviados += chatConContacto.cantidadTotalMensajesEnviados();
		}
		return mensajesEnviados;
	}

	@Override
	public boolean existeChat(String nombre) {
		return getContacto(nombre) != null;
	}

	@Override
	public List<String> obtenerConversacionCon(String nombre) {
		return getContacto(nombre).obtenerConversacion();
	}

	@Override
	public void enviarMensajeA(String nombre, String mensaje) {
		getContacto(nombre).enviarMensaje(mensaje);
	}

	@Override
	public void borrarMensajes(String nombre) {
		getContacto(nombre).borrarMensajes();
	}
	
}
