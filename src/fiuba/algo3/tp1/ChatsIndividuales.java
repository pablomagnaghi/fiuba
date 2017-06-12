package fiuba.algo3.tp1;

import java.util.ArrayList;
import java.util.List;

public class ChatsIndividuales extends Chats {

	private List<Contacto> contactos;

	public ChatsIndividuales() {
		contactos = new ArrayList<Contacto>();
	}

	public int cantidadDeContactos() {
		return contactos.size();
	}

	public void agregarContacto(String nombreContacto) {
		// TODO Auto-generated method stub
		
	}
	
	public void recibirMensajeDe(String nombreContacto, String mensaje) {
		// TODO Auto-generated method stub
		
	}

	public int cantidadMensajesDe(String nombreContacto) {
		// TODO Auto-generated method stub
		return 0;
	}

	public int cantidadMensajesEnviadosA(String nombreContacto) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int cantidadDeChats() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int cantidadDeMensajesEnviados() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int cantidadTotalMensajesRecibidos() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int cantidadTotalMensajesEnviados() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public boolean existeChat(String nombreChat) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public List<String> obtenerConversacionCon(String nombreChat) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void enviarMensajeA(String nombreChat, String mensaje) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void borrarMensajes(String nombreChat) {
		// TODO Auto-generated method stub
		
	}

	

	
}
