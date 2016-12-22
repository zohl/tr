import Inferno from 'inferno';

export default function category(c) {
  return (
    <label className = "category">
      <input type = "radio" name = "category" value = {c.name}/>
      <p>name: {c.name}</p>
      <p>description: {c.description}</p>
    </label>
  );
}

